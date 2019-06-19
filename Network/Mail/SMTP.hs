{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

module Network.Mail.SMTP
    ( -- * Main interface
      sendMail
    , sendMail'
    , sendMailWithLogin
    , sendMailWithLogin'
    , sendMailWithSender
    , sendMailWithSender'
    , simpleMail
    , plainTextPart
    , htmlPart
    , filePart

    -- * Types
    , module Network.Mail.SMTP.Types
    , SMTPConnection

      -- * Network.Mail.Mime's sendmail interface (reexports)
    , sendmail
    , sendmailCustom
    , renderSendMail
    , renderSendMailCustom

      -- * Establishing Connection
    , connectSMTP
    , connectSMTP'
    , connectSMTPWithHostName

      -- * Operation to a Connection
    , sendCommand
    , login
    , closeSMTP
    , renderAndSend
    , renderAndSendFrom
    )
    where

import Network.Mail.SMTP.Auth
import Network.Mail.SMTP.Types

import System.FilePath (takeFileName)

import Control.Monad (unless)
import Data.Char (isDigit)

import Network.Socket
import Network.BSD (getHostName)
import Network.Mail.Mime hiding (filePart, htmlPart, simpleMail)
import qualified Network.Connection as Conn

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Text.Encoding

data SMTPConnection = SMTPC !Conn.Connection ![ByteString]

instance Eq SMTPConnection where
    (==) (SMTPC a _) (SMTPC b _) = Conn.connectionID a == Conn.connectionID b

-- | Connect to an SMTP server with the specified host and default port (25)
connectSMTP :: HostName     -- ^ name of the server
            -> IO SMTPConnection
connectSMTP hostname = connectSMTP' hostname 25

-- | Connect to an SMTP server with the specified host and port
connectSMTP' :: HostName     -- ^ name of the server
             -> PortNumber -- ^ port number
             -> IO SMTPConnection
connectSMTP' hostname port = do
    context <- Conn.initConnectionContext
    Conn.connectTo context connParams >>= connectStream getHostName
  where
    connParams = Conn.ConnectionParams hostname port Nothing Nothing


-- | Connect to an SMTP server with the specified host and port
connectSMTPWithHostName :: HostName     -- ^ name of the server
                        -> PortNumber -- ^ port number
                        -> IO String -- ^ Returns the host name to use to send from
                        -> IO SMTPConnection
connectSMTPWithHostName hostname port getMailHostName = do
    context <- Conn.initConnectionContext
    Conn.connectTo context connParams >>= connectStream getMailHostName
  where
    connParams = Conn.ConnectionParams hostname port Nothing Nothing


-- | Attemp to send a 'Command' to the SMTP server once
tryOnce :: SMTPConnection -> Command -> ReplyCode -> IO ByteString
tryOnce = tryCommand 1

-- | Repeatedly attempt to send a 'Command' to the SMTP server
tryCommand :: Int -> SMTPConnection -> Command -> ReplyCode
           -> IO ByteString
tryCommand tries st cmd expectedReply = do
    (code, msg) <- tryCommandNoFail tries st cmd expectedReply
    if code == expectedReply
      then return msg
      else do
        closeSMTP st
        fail $ "Unexpected reply to: " ++ show cmd ++
          ", Expected reply code: " ++ show expectedReply ++
          ", Got this instead: " ++ show code ++ " " ++ show msg

tryCommandNoFail :: Int -> SMTPConnection -> Command -> ReplyCode
                 -> IO (ReplyCode, ByteString)
tryCommandNoFail tries st cmd expectedReply = do
  (code, msg) <- sendCommand st cmd
  if code == expectedReply
    then return (code, msg)
    else if tries > 1
      then tryCommandNoFail (tries - 1) st cmd expectedReply
      else return (code, msg)

-- | Create an 'SMTPConnection' from an already connected Handle
connectStream :: IO String -> Conn.Connection -> IO SMTPConnection
connectStream getMailHostName st = do
    (code1, _) <- parseResponse st
    unless (code1 == 220) $ do
        Conn.connectionClose st
        fail "cannot connect to the server"
    senderHost <- getMailHostName
    (code, initialMsg) <- tryCommandNoFail 3 (SMTPC st []) (EHLO $ B8.pack senderHost) 250
    if code == 250
      then return (SMTPC st (tail $ B8.lines initialMsg))
      else do -- EHLO failed, try HELO
        msg <- tryCommand 3 (SMTPC st []) (HELO $ B8.pack senderHost) 250
        return (SMTPC st (tail $ B8.lines msg))

parseResponse :: Conn.Connection -> IO (ReplyCode, ByteString)
parseResponse conn = do
    (code, bdy) <- readLines
    return (read $ B8.unpack code, B8.unlines bdy)
  where
    readLines = do
      l <- Conn.connectionGetLine 1000 conn
      let (c, bdy) = B8.span isDigit l
      if not (B8.null bdy) && B8.head bdy == '-'
         then do (c2, ls) <- readLines
                 return (c2, B8.tail bdy:ls)
         else return (c, [B8.tail bdy])


-- | Send a 'Command' to the SMTP server
sendCommand :: SMTPConnection -> Command -> IO (ReplyCode, ByteString)

sendCommand (SMTPC conn _) (DATA dat) = do
    bsPutCrLf conn "DATA"
    (code, _) <- parseResponse conn
    unless (code == 354) $ fail "this server cannot accept any data."
    mapM_ sendLine $ split dat
    sendLine dot
    parseResponse conn
  where
    sendLine = bsPutCrLf conn
    split = map (padDot . stripCR) . B8.lines
    -- remove \r at the end of a line
    stripCR s = if cr `B8.isSuffixOf` s then B8.init s else s
    -- duplicate . at the start of a line
    padDot s = if dot `B8.isPrefixOf` s then dot <> s else s
    cr = B8.pack "\r"
    dot = B8.pack "."

sendCommand (SMTPC conn _) (AUTH LOGIN username password) = do
    bsPutCrLf conn command
    _ <- parseResponse conn
    bsPutCrLf conn userB64
    _ <- parseResponse conn
    bsPutCrLf conn passB64
    (code, msg) <- parseResponse conn
    unless (code == 235) $ fail "authentication failed."
    return (code, msg)
  where
    command = "AUTH LOGIN"
    (userB64, passB64) = encodeLogin username password

sendCommand (SMTPC conn _) (AUTH at username password) = do
    bsPutCrLf conn command
    (code, msg) <- parseResponse conn
    unless (code == 334) $ fail "authentication failed."
    bsPutCrLf conn $ auth at (B8.unpack msg) username password
    parseResponse conn
  where
    command = B8.pack $ unwords ["AUTH", show at]

sendCommand (SMTPC conn _) meth = do
    bsPutCrLf conn command
    parseResponse conn
  where
    command = case meth of
        (HELO param) -> "HELO " <> param
        (EHLO param) -> "EHLO " <> param
        (MAIL param) -> "MAIL FROM:<" <> param <> ">"
        (RCPT param) -> "RCPT TO:<" <> param <> ">"
        (EXPN param) -> "EXPN " <> param
        (VRFY param) -> "VRFY " <> param
        (HELP msg)   -> if B8.null msg
                          then "HELP\r\n"
                          else "HELP " <> msg
        NOOP         -> "NOOP"
        RSET         -> "RSET"
        QUIT         -> "QUIT"
        DATA{}       ->
            error "BUG: DATA pattern should be matched by sendCommand patterns"
        AUTH{}       ->
            error "BUG: AUTH pattern should be matched by sendCommand patterns"


-- | Send 'QUIT' and close the connection.
closeSMTP :: SMTPConnection -> IO ()
closeSMTP c@(SMTPC conn _) = sendCommand c QUIT >> Conn.connectionClose conn

-- | Sends a rendered mail to the server.
sendRenderedMail :: ByteString   -- ^ sender mail
            -> [ByteString] -- ^ receivers
            -> ByteString   -- ^ data
            -> SMTPConnection
            -> IO ()
sendRenderedMail sender receivers dat conn = do
    _ <- tryOnce conn (MAIL sender) 250
    mapM_ (\r -> tryOnce conn (RCPT r) 250) receivers
    _ <- tryOnce conn (DATA dat) 250
    return ()

-- | Render a 'Mail' to a 'ByteString' then send it over the specified
-- 'SMTPConnection'
renderAndSend ::SMTPConnection -> Mail -> IO ()
renderAndSend conn mail@Mail{..} = do
    rendered <- lazyToStrict `fmap` renderMail' mail
    sendRenderedMail from to rendered conn
  where enc  = encodeUtf8 . addressEmail
        from = enc mailFrom
        to   = map enc $ mailTo ++ mailCc ++ mailBcc

-- | Connect to an SMTP server, send a 'Mail', then disconnect.  Uses the default port (25).
sendMail :: HostName -> Mail -> IO ()
sendMail host mail = do
  con <- connectSMTP host
  renderAndSend con mail
  closeSMTP con

-- | Connect to an SMTP server, send a 'Mail', then disconnect.
sendMail' :: HostName -> PortNumber -> Mail -> IO ()
sendMail' host port mail = do
  con <- connectSMTP' host port
  renderAndSend con mail
  closeSMTP con

-- | Connect to an SMTP server, login, send a 'Mail', disconnect.  Uses the default port (25).
sendMailWithLogin :: HostName -> UserName -> Password -> Mail -> IO ()
sendMailWithLogin host user pass mail = do
  con <- connectSMTP host
  _ <- sendCommand con (AUTH LOGIN user pass)
  renderAndSend con mail
  closeSMTP con

-- | Connect to an SMTP server, login, send a 'Mail', disconnect.
sendMailWithLogin' :: HostName -> PortNumber -> UserName -> Password -> Mail -> IO ()
sendMailWithLogin' host port user pass mail = do
  con <- connectSMTP' host port
  _ <- sendCommand con (AUTH LOGIN user pass)
  renderAndSend con mail
  closeSMTP con

-- | Send a 'Mail' with a given sender.
sendMailWithSender :: ByteString -> HostName -> Mail -> IO ()
sendMailWithSender sender host mail = do
    con <- connectSMTP host
    renderAndSendFrom sender con mail
    closeSMTP con

-- | Send a 'Mail' with a given sender.
sendMailWithSender' :: ByteString -> HostName -> PortNumber -> Mail -> IO ()
sendMailWithSender' sender host port mail = do
    con <- connectSMTP' host port
    renderAndSendFrom sender con mail
    closeSMTP con

renderAndSendFrom :: ByteString -> SMTPConnection -> Mail -> IO ()
renderAndSendFrom sender conn mail@Mail{..} = do
    rendered <- BL.toStrict `fmap` renderMail' mail
    sendRenderedMail sender to rendered conn
  where enc  = encodeUtf8 . addressEmail
        to   = map enc $ mailTo ++ mailCc ++ mailBcc

-- | A convenience function that sends 'AUTH' 'LOGIN' to the server
login :: SMTPConnection -> UserName -> Password -> IO (ReplyCode, ByteString)
login con user pass = sendCommand con (AUTH LOGIN user pass)

-- | A simple interface for generating a 'Mail' with a plantext body and
-- an optional HTML body.
simpleMail :: Address   -- ^ from
           -> [Address] -- ^ to
           -> [Address] -- ^ CC
           -> [Address] -- ^ BCC
           -> T.Text -- ^ subject
           -> [Part] -- ^ list of parts (list your preferred part last)
           -> Mail
simpleMail from to cc bcc subject parts =
    Mail { mailFrom = from
         , mailTo   = to
         , mailCc   = cc
         , mailBcc  = bcc
         , mailHeaders = [ ("Subject", subject) ]
         , mailParts = [parts]
         }

-- | Construct a plain text 'Part'
plainTextPart :: TL.Text -> Part
plainTextPart body = Part "text/plain; charset=utf-8"
              QuotedPrintableText DefaultDisposition [] (PartContent $ TL.encodeUtf8 body)
{-# DEPRECATED plainTextPart "Use plainPart from mime-mail package" #-}

-- | Construct an html 'Part'
htmlPart :: TL.Text -> Part
htmlPart body = Part "text/html; charset=utf-8"
             QuotedPrintableText DefaultDisposition [] (PartContent $ TL.encodeUtf8 body)
{-# DEPRECATED htmlPart "Use htmlPart from mime-mail package" #-}

-- | Construct a file attachment 'Part'
filePart :: T.Text -- ^ content type
         -> FilePath -- ^ path to file
         -> IO Part
filePart ct fp = do
    content <- BL.readFile fp
    return $ Part ct Base64 (AttachmentDisposition $ T.pack (takeFileName fp)) [] (PartContent content)
{-# DEPRECATED filePart "Use filePart from mime-mail package" #-}

lazyToStrict :: BL.ByteString -> B.ByteString
lazyToStrict = B.concat . BL.toChunks

crlf :: B8.ByteString
crlf = B8.pack "\r\n"

bsPutCrLf :: Conn.Connection -> ByteString -> IO ()
bsPutCrLf conn = Conn.connectionPut conn . flip B.append crlf
