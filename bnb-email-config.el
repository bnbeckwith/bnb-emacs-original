
(setq socks-override-functions 1)
(setq socks-noproxy '("*.intel.com"))
(require 'socks)

;; Sending mail GNUS
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtp-auth-credentials '(("smtp.gmail.com" 587 "bnbeckwith@gmail.com" nil)))
(setq smtpmail-default-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 587)
