module Network.Mail.SMTP.Auth (
    UserName,
    Password,
    Token,
    AuthType(..),
    encodeLogin,
    encodeLoginOAuth,
    auth,
) where

import Crypto.MAC.HMAC (hmac, HMAC)
import Crypto.Hash.Algorithms (MD5)
import Data.ByteArray (copyAndFreeze)
import qualified Data.ByteString.Base16 as B16  (encode)
import qualified Data.ByteString.Base64 as B64  (encode)

import Data.ByteString  (ByteString)
import Data.List
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8    (unwords)

type UserName = String
type Password = String
type Token = String

data AuthType
    = PLAIN
    | LOGIN
    | LOGIN_OAUTH
    | CRAM_MD5
    deriving Eq

instance Show AuthType where
    showsPrec d at = showParen (d>app_prec) $ showString $ showMain at
        where app_prec = 10
              showMain PLAIN       = "PLAIN"
              showMain LOGIN       = "LOGIN"
              showMain LOGIN_OAUTH = "XOAUTH2"
              showMain CRAM_MD5    = "CRAM-MD5"

toAscii :: String -> ByteString
toAscii = B.pack . map (toEnum.fromEnum)

b64Encode :: String -> ByteString
b64Encode = B64.encode . toAscii

hmacMD5 :: ByteString -> ByteString -> ByteString
hmacMD5 text key =
    let mac = hmac key text :: HMAC MD5
    in copyAndFreeze mac (const $ return ())

encodePlain :: UserName -> Password -> ByteString
encodePlain user pass = b64Encode $ intercalate "\0" [user, user, pass]

encodeLogin :: UserName -> Password -> (ByteString, ByteString)
encodeLogin user pass = (b64Encode user, b64Encode pass)

-- | Encode the xoauth 2 message based on:
-- https://docs.microsoft.com/en-us/exchange/client-developer/legacy-protocols/how-to-authenticate-an-imap-pop-smtp-application-by-using-oauth#sasl-xoauth2
encodeLoginOAuth :: UserName -> Token -> ByteString
encodeLoginOAuth user oauthToken =
  b64Encode ("user=" <> user <> "\x01" <> "auth=Bearer " <> oauthToken <> "\x01\x01")

cramMD5 :: String -> UserName -> Password -> ByteString
cramMD5 challenge user pass =
    B64.encode $ B8.unwords [user', B16.encode (hmacMD5 challenge' pass')]
  where
    challenge' = toAscii challenge
    user'      = toAscii user
    pass'      = toAscii pass

auth :: AuthType -> String -> UserName -> Password -> ByteString
auth PLAIN       _ u p = encodePlain u p
auth LOGIN       _ u p = let (u', p') = encodeLogin u p in B8.unwords [u', p']
auth LOGIN_OAUTH _ u t = encodeLoginOAuth u t
auth CRAM_MD5    c u p = cramMD5 c u p
