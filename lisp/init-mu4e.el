;;; #init-mu4e.el --- mu4e config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(when (and (not (eq system-type 'windows-nt))
           (not (string-equal (system-name) "jco")))

  (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
  (require 'mu4e-context)

  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "Gmail"
            :enter-func (lambda ()
                          (mu4e-message "Switch to the Gmail context"))
            ;; leave-func not defined
            :match-func (lambda (msg)
                          (if msg
                              (mu4e-message-contact-field-matches
                               msg :to "jonas.collberg@gmail.com")
                            (not (jco/at-office-p))))
            :vars '((user-mail-address . "jonas.collberg@gmail.com")
                    ;; (mu4e-compose-signature . "Jonas\n")
                    (mu4e-drafts-folder . "/gmail/Drafts")
                    (mu4e-sent-folder . "/gmail/Sent")
                    (mu4e-trash-folder . "/gmail/Trash")
                    (mu4e-maildir-shortcuts . (("/gmail/Inbox" . ?i)
                                               ("/gmail/Sent" . ?s)
                                               ("/gmail/Trash" . ?t)))
                    (mu4e-completing-read-function . compl-fun)))
          ,(make-mu4e-context
            :name "Kolab Now"
            :enter-func (lambda ()
                          (mu4e-message "Switch to the Kolabnow context"))
            ;; leave-func not defined
            :match-func (lambda (msg)
                          (if msg
                              (mu4e-message-contact-field-matches
                               msg :to "jonas.collberg@mykolab.com")
                            (not (jco/at-office-p))))
            :vars '((user-mail-address . "jonas.collberg@mykolab.com")
                    ;; (mu4e-compose-signature . "Jonas\n")
                    (mu4e-drafts-folder . "/kolabnow/Drafts")
                    (mu4e-sent-folder . "/kolabnow/Sent")
                    (mu4e-trash-folder . "/kolabnow/Trash")
                    (mu4e-maildir-shortcuts . (("/kolabnow/Inbox" . ?i)
                                               ("/kolabnow/Sent" . ?s)
                                               ("/kolabnow/Trash" . ?t)))
                    (mu4e-completing-read-function . compl-fun)))
          ,(make-mu4e-context
            :name "Work"
            :enter-func (lambda () (mu4e-message "Switch to the Work context"))
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
                    (mu4e-drafts-folder . "/zimpler/[Gmail].Drafts")
                    (mu4e-sent-folder . "/zimpler/[Gmail].Sent Mail")
                    (mu4e-trash-folder . "/zimpler/[Gmail].Trash")
                    (mu4e-maildir-shortcuts .
                                            (("/zimpler/Inbox" . ?i)
                                             ("/zimpler/[Gmail].Sent Mail" . ?s)
                                             ("/zimpler/[Gmail].Trash" . ?t)
                                             ("/zimpler/[Gmail].All Mail" . ?a)))
                    (mu4e-completing-read-function . compl-fun)))))

  (add-hook 'mu4e-main-mode-hook
            (lambda ()
              (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
              (require 'mu4e)
              ;; (require 'mu4e-vars)
              (require 'imapfilter)
              (setq mu4e-maildir "~/.mail")
              (dolist (m (list mu4e-main-mode-map
                               mu4e-headers-mode-map
                               mu4e-view-mode-map))
                (define-key m "\C-w" 'evil-window-map))
              (setq mu4e-get-mail-command "mbsync -a")
              (setq mu4e-update-interval nil)
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

              (defun compl-fun (prompt maildirs predicate require-match initial-input)
                (helm-comp-read prompt maildirs
                                :name prompt
                                :must-match t))

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
