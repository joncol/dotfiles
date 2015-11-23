(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))

(setq gnus-secondary-select-methods
      '((nnimap "orzone"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl))
        ;; (nntp "news.newshosting.com")
        ))

(setq smtpmail-smtp-service 587)
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq send-mail-function    'smtpmail-send-it
      smtpmail-smtp-user    "jonas.collberg@orzone.com"
      smtpmail-smtp-server  "smtp.gmail.com"
      smtpmail-stream-type  'starttls
      smtpmail-smtp-service 587)

;; (require 'smtpmail)
;; (require 'smtpmail-multi)
;; (setq smtpmail-auth-credentials "~/.authinfo")
;; (setq smtpmail-multi-accounts '((orzone . '("jonas.collberg@orzone.com" "smtp.gmail.com"
;;                                             587 "jonas.collberg@orzone.com"
;;                                             ssl nil nil nil))
;;                                 (gmail . '("jonas.collberg@gmail.com" "smtp.gmail.com"
;;                                            587 "jonas.collberg@gmail.com"
;;                                            ssl nil nil nil))))
;; (setq smtpmail-multi-associations
;;       '(("jonas.collberg@orzone.com" orzone)
;;         ("jonas.collberg@gmail.com" gmail)))

;; (setq smtpmail-multi-default-account 'orzone)
;; (setq message-send-mail-function 'smtpmail-multi-send-it)
