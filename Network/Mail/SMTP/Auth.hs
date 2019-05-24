module Network.Mail.SMTP.Auth (
    UserName,
    Password,
    AuthType(..),
    encodeLogin,
    auth,
) where

import Crypto.Hash.MD5 (hash)
import qualified Data.ByteString.Base16 as B16  (encode)
import qualified Data.ByteString.Base64 as B64  (encode)

import Data.ByteString  (ByteString)
import Data.List
import Data.Bits
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8    (unwords)

type UserName = String
type Password = String

data AuthType
    = PLAIN
    | LOGIN
    | CRAM_MD5
    deriving Eq

instance Show AuthType where
    showsPrec d at = showParen (d>app_prec) $ showString $ showMain at
        where app_prec = 10
              showMain PLAIN    = "PLAIN"
              showMain LOGIN    = "LOGIN"
              showMain CRAM_MD5 = "CRAM-MD5"

toAscii :: String -> ByteString
toAscii = B.pack . map (toEnum.fromEnum)

b64Encode :: String -> ByteString
b64Encode = B64.encode . toAscii

hmacMD5 :: ByteString -> ByteString -> ByteString
hmacMD5 text key = hash (okey <> hash (ikey <> text))
    where key' = if B.length key > 64
                 then hash key <> B.replicate 48 0
                 else key <> B.replicate (64-B.length key) 0
          ipad = B.replicate 64 0x36
          opad = B.replicate 64 0x5c
          ikey = B.pack $ B.zipWith xor key' ipad
          okey = B.pack $ B.zipWith xor key' opad

encodePlain :: UserName -> Password -> ByteString
encodePlain user pass = b64Encode $ intercalate "\0" [user, user, pass]

encodeLogin :: UserName -> Password -> (ByteString, ByteString)
encodeLogin user pass = (b64Encode user, b64Encode pass)

cramMD5 :: String -> UserName -> Password -> ByteString
cramMD5 challenge user pass =
    B64.encode $ B8.unwords [user', B16.encode (hmacMD5 challenge' pass')]
  where
    challenge' = toAscii challenge
    user'      = toAscii user
    pass'      = toAscii pass

auth :: AuthType -> String -> UserName -> Password -> ByteString
auth PLAIN    _ u p = encodePlain u p
auth LOGIN    _ u p = let (u', p') = encodeLogin u p in B8.unwords [u', p']
auth CRAM_MD5 c u p = cramMD5 c u p
