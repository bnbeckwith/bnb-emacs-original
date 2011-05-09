
;; Sending mail GNUS
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtp-auth-credentials '(("smtp.gmail.com" 587 "bnbeckwith@gmail.com" nil)))
(setq smtpmail-default-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 587)
