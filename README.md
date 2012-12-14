SMTP-MAIL
=========

Making it easy to send SMTP emails from Haskell.

```haskell
import Network.Mail.SMTP

from    = Address Nothing "email@domain.com"
to      = Address (Just "Jason Hickner") "email@domain.com"
subject = "email subject"
body    = "email body"
html    = "<h1>HTML</h1>" -- optional html body

main = sendMail host port $ simpleMail from [to] subject body (Just html)
```

or, with authentication:

```sendMailWithLogin host port user pass $ ...```

For more complicated scenarios or for adding attachments you can import
```Network.Mail.Mime``` and construct ```Mail``` objects manually.


### Thanks

This library is based on code from HaskellNet, which appears to be no longer
maintained. I've cleaned up the error handling, added some API functions to
make common operations easier, and switched to ByteStrings where applicable.
