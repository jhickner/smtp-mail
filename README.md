SMTP-MAIL
=========

Making it easy to send SMTP emails from Haskell.

### Sending with an SMTP server

```haskell
import Network.Mail.SMTP

from       = Address Nothing "email@domain.com"
to         = [Address (Just "Jason Hickner") "email@domain.com"]
cc         = []
bcc        = []
subject    = "email subject"
body       = plainTextPart "email body"
html       = htmlPart "<h1>HTML</h1>"
attachment = filePart "path/to/attachment.zip"

mail = simpleMail from to cc bcc subject [body, html, attachment]

main = sendMail host port mail
```

or, with authentication:

```haskell
main = sendMailWithLogin host port user pass mail
```

### Sending with sendmail

If you'd like to use sendmail, the sendmail interface from ```Network.Mail.Mime``` 
is reexported as well:

```haskell
-- send via the default sendmail executable with default options
renderSendMail mail

-- send via the specified executable with specified options
renderSendMailCustom filepath [opts] mail
```

For more complicated scenarios or for adding attachments or CC/BCC
addresses you can import ```Network.Mail.Mime``` and construct ```Mail```
objects manually.


### Thanks

This library is based on code from HaskellNet, which appears to be no longer
maintained. I've cleaned up the error handling, added some API functions to
make common operations easier, and switched to ByteStrings where applicable.
