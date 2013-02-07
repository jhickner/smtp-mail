module Network.Mail.SMTP.Types (
    Command(..),
    ReplyCode,
    Response(..),

    -- * Auth types (re-exports)
    UserName,
    Password,
    AuthType(..),

    -- * "Network.Mail.Mime" types (re-exports)
    Address(..),
) where

import Network.Mail.SMTP.Auth

import Data.ByteString (ByteString)
import Network.Mail.Mime

data Command
    = HELO ByteString
    | EHLO ByteString
    | MAIL ByteString
    | RCPT ByteString
    | DATA ByteString
    | EXPN ByteString
    | VRFY ByteString
    | HELP ByteString
    | AUTH AuthType UserName Password
    | NOOP
    | RSET
    | QUIT
    deriving (Show, Eq)

type ReplyCode = Int

data Response
    = Ok
    | SystemStatus
    | HelpMessage
    | ServiceReady
    | ServiceClosing
    | UserNotLocal
    | CannotVerify
    | StartMailInput
    | ServiceNotAvailable
    | MailboxUnavailable
    | ErrorInProcessing
    | InsufficientSystemStorage
    | SyntaxError
    | ParameterError
    | CommandNotImplemented
    | BadSequence
    | ParameterNotImplemented
    | MailboxUnavailableError
    | UserNotLocalError
    | ExceededStorage
    | MailboxNotAllowed
    | TransactionFailed
    deriving (Show, Eq)
