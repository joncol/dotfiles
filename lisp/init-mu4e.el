;;; #init-mu4e.el --- mu4e config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(when (and (not (eq system-type 'windows-nt))
           (not (string-equal system-name "jco")))

  (require 'mu4e-context)

  (setq mu4e-contexts
        `( ,(make-mu4e-context
            :name "Private"
            :enter-func (lambda ()
                          (mu4e-message "Switch to the Private context"))
            ;; leave-func not defined
            :match-func (lambda (msg)
                          (if msg
                              (mu4e-message-contact-field-matches
                               msg :to "jonas.collberg@mykolab.com")
                            (not (jco/at-office-p))))
            :vars '((user-mail-address . "jonas.collberg@mykolab.com")
                    ;; (mu4e-compose-signature . "Jonas\n")
                    (mu4e-drafts-folder . "/personal_mykolab/Drafts")
                    (mu4e-sent-folder . "/personal_mykolab/Sent")
                    (mu4e-trash-folder . "/personal_mykolab/Trash")
                    (mu4e-maildir-shortcuts .
                                            (("/personal_mykolab/INBOX" . ?i)
                                             ("/personal_mykolab/Sent" . ?s)
                                             ("/personal_mykolab/Trash" . ?t)))
                    (mu4e-completing-read-function . compl-fun)
                    ))
          ,(make-mu4e-context
            :name "Work"
            :enter-func (lambda () (mu4e-message
                                    "Switch to the Work context"))
            ;; leave-fun not defined
            :match-func (lambda (msg)
                          (if msg
                              (mu4e-message-contact-field-matches
                               msg :to "jonas.collberg@zimpler.com")
                            (jco/at-office-p)))
            :vars '((user-mail-address . "jonas.collberg@zimpler.com")
                    ;; (mu4e-compose-signature . (concat
                    ;;                             "Kind regards,\n"
                    ;;                             user-full-name))
                    (mu4e-drafts-folder . "/zimpler_gmail/[Gmail].Drafts")
                    (mu4e-sent-folder . "/zimpler_gmail/[Gmail].Sent Mail")
                    (mu4e-trash-folder . "/zimpler_gmail/[Gmail].Trash")
                    (mu4e-maildir-shortcuts .
                                            (("/zimpler_gmail/INBOX" . ?i)
                                             ("/zimpler_gmail/[Gmail].Sent Mail" . ?s)
                                             ("/zimpler_gmail/[Gmail].Trash" . ?t)
                                             ("/zimpler_gmail/[Gmail].All Mail" . ?a)))
                    (mu4e-completing-read-function . compl-fun)))))

  (add-hook 'mu4e-main-mode-hook
            (lambda ()
              (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

              (require 'mu4e)
              (require 'imapfilter)

              (setq mu4e-get-mail-command "offlineimap")
              (setq mu4e-update-interval 120)
              (setq mu4e-sent-messages-behavior 'sent)
              (setq mu4e-html2text-command "w3m -T text/html")
              (setq mu4e-view-show-images t)
              (setq mu4e-view-show-addresses t)
              (add-to-list 'mu4e-view-actions '("ViewInBrowser" .
                                                mu4e-action-view-in-browser) t)
              (setq mu4e-view-show-addresses t)

              (setq mu4e-compose-context-policy 'always-ask)
              (setq mu4e-compose-in-new-frame t)
              (setq mu4e-save-multiple-attachments-without-asking t)
              (setq mu4e-compose-format-flowed t)
              (setq mu4e-compose-dont-reply-to-self t)

              (setq mu4e-headers-date-format "%Y-%m-%d %H:%M")
              (setq mu4e-headers-fields
                    '((:date    . 25)
                      (:flags   .  6)
                      (:from    . 22)
                      (:subject . nil)))

              (defun jco/smtp-server ()
                (cond ((or (s-contains? "gmail.com" user-mail-address)
                           (s-contains? "zimpler.com" user-mail-address)) "smtp.gmail.com")
                      ((s-contains? "kolab" user-mail-address) "smtp.kolabnow.com")))

              (defun jco/my-send-it ()
                (setq smtpmail-starttls-credentials `((,(jco/smtp-server) 587 nil nil))
                      smtpmail-auth-credentials
                      `((,(jco/smtp-server) 587 user-mail-address nil))
                      smtpmail-default-smtp-server (jco/smtp-server)
                      smtpmail-smtp-server (jco/smtp-server))
                (smtpmail-send-it))

              (require 'smtpmail)

              (setq message-send-mail-function 'jco/my-send-it
                    starttls-use-gnutls t
                    smtpmail-smtp-service 587)

              ;; don't keep message buffers around
              (setq message-kill-buffer-on-exit t)

              (setq mu4e-org-contacts-file "~/.contacts")
              (add-to-list 'mu4e-headers-actions
                           '("org-contact-add" . mu4e-action-add-org-contact) t)
              (add-to-list 'mu4e-view-actions
                           '("org-contact-add" . mu4e-action-add-org-contact) t))))

  (add-hook 'mu4e-update-pre-hook
            #'imapfilter)

  (add-hook 'mu4e-view-mode-hook
            (lambda ()
              (mu4e-view-fill-long-lines)))

  (add-hook 'mu4e-compose-mode-hook
            (lambda ()
              (fci-mode)
              (ethan-wspace-mode -1)
              (turn-off-auto-fill)
              (footnote-mode)
              (setq truncate-lines nil)
              (setq word-wrap t)))

(provide 'init-mu4e)

;;; init-mu4e.el ends here
