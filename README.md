SMTP-MAIL
=========

Making it easy to send SMTP emails from Haskell.

```
cabal install smtp-mail
```

### Sending with an SMTP server

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Network.Mail.SMTP

from       = Address Nothing "email@domain.com"
to         = [Address (Just "Jason Hickner") "email@domain.com"]
cc         = []
bcc        = []
subject    = "email subject"
body       = plainTextPart "email body"
html       = htmlPart "<h1>HTML</h1>"

mail = simpleMail from to cc bcc subject [body, html]

main = sendMail host mail
```

or with an attachment:

```haskell
main = do
  attachment <- filePart "application/octet-stream" "path/to/attachment.zip"
  let mail = simpleMail from to cc bcc subject [body, html, attachment]
  sendMail host mail
```

or, with authentication:

```haskell
main = sendMailWithLogin host user pass mail
```

Note: `sendMail'` and `sendMailWithLogin'` variations are also provided if you want to specify a port as well as a hostname.


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

### Caveat

You will encounter authentication errors if you try to connect to an SMTP server that expects SSL. If that's what you're looking to do, try [HaskellNet-SSL](http://hackage.haskell.org/package/HaskellNet-SSL).

### Thanks

This library is based on code from HaskellNet, which appears to be no longer
maintained. I've cleaned up the error handling, added some API functions to
make common operations easier, and switched to ByteStrings where applicable.
