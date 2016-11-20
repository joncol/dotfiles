(when (not (member system-type '(windows-nt darwin)))
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

  (require 'mu4e)

  (setq mu4e-get-mail-command "offlineimap")
  (setq mu4e-update-interval 120)
  (setq mu4e-sent-messages-behavior 'sent)
  (setq mu4e-html2text-command "w3m -T text/html")
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses t)
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" .
                                    mu4e-action-view-in-browser) t)

  (setq mu4e-compose-context-policy 'always-ask)

  (defun compl-fun (prompt maildirs predicate require-match initial-input)
    (helm-comp-read prompt maildirs
                    :name prompt
                    :must-match t))

  (defun jco/smtp-server ()
    (cond ((or (s-contains? "gmail.com" user-mail-address)
               (s-contains? "orzone.com" user-mail-address)) "smtp.gmail.com")
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
              (setq word-wrap t)
              ;; (setq use-hard-newlines t)
              ))

  (setq mu4e-org-contacts-file "~/.contacts")
  (add-to-list 'mu4e-headers-actions
               '("org-contact-add" . mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions
               '("org-contact-add" . mu4e-action-add-org-contact) t)

  ;; (add-hook 'message-send-hook
  ;;           (lambda ()
  ;;             (mu4e-send-harden-newlines)))

  ;; (defun mu4e-send-harden-newlines ()
  ;;   "Set the hard property to all newlines."
  ;;   (save-excursion
  ;;     (goto-char (point-min))
  ;;     (while (search-forward "\n" nil t)
  ;;       (put-text-property (1- (point)) (point) 'hard t))))

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
             :enter-func (lambda () (mu4e-message "Switch to the Work context"))
             ;; leave-fun not defined
             :match-func (lambda (msg)
                           (if msg
                               (mu4e-message-contact-field-matches
                                msg :to "jonas.collberg@orzone.com")
                             (jco/at-office-p)))
             :vars '((user-mail-address . "jonas.collberg@orzone.com")
                     ;; (mu4e-compose-signature . (concat
                     ;;                             "Kind regards,\n"
                     ;;                             user-full-name))
                     (mu4e-drafts-folder . "/work_gmail/[Gmail].Drafts")
                     (mu4e-sent-folder . "/work_gmail/[Gmail].Sent Mail")
                     (mu4e-trash-folder . "/work_gmail/[Gmail].Trash")
                     (mu4e-maildir-shortcuts .
                      (("/work_gmail/INBOX" . ?i)
                       ("/work_gmail/[Gmail].Sent Mail" . ?s)
                       ("/work_gmail/[Gmail].Trash" . ?t)
                       ("/work_gmail/[Gmail].All Mail" . ?a)))
                     (mu4e-completing-read-function . compl-fun)
                     ))))

  ;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e
  ;; should guess or ask the correct context, e.g.

  ;; start with the first (default) context;
  ;; default is to ask-if-none (ask when there's no context yet, and none match)
  ;; (setq mu4e-context-policy 'pick-first)

  ;; compose with the current context is no context matches;
  ;; default is to ask
  ;; '(setq mu4e-compose-context-policy nil)
  )

(provide 'init-mu4e)
