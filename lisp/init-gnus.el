(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)))
;; (setq gnus-secondary-select-methods '((nntp "news-europe.giganews.com")))

(setq smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq smtpmail-smtp-server "smtp.gmail.com")
