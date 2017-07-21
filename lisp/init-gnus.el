;;; #init-gnus.el --- Gnus config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

(setq gnus-select-method
      '(nnimap "gmail.com"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))

(setq gnus-secondary-select-methods
      '((nnimap "zimpler.com"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl))))

(setq smtpmail-smtp-service 587)
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq send-mail-function    'smtpmail-send-it
      smtpmail-smtp-user    user-mail-address
      smtpmail-smtp-server  "smtp.gmail.com"
      smtpmail-stream-type  'starttls
      smtpmail-smtp-service 587)

(provide 'init-gnus)

;;; init-gnus.el ends here
