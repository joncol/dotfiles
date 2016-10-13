(when (not (eq system-type 'windows-nt))
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
  (require 'mu4e)

  ;; default
  ;; (setq mu4e-maildir "~/Maildir")

  (setq mu4e-drafts-folder "/Drafts")
  (setq mu4e-sent-folder   "/Sent")
  (setq mu4e-trash-folder  "/Trash")

  (setq mu4e-update-interval 120)

  ;; don't save message to Sent Messages, Mykolab/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)

  ;; (See the documentation for `mu4e-sent-messages-behavior' if you have
  ;; additional non-Mykolab addresses and want assign them different
  ;; behavior.)

  ;; setup some handy shortcuts
  ;; you can quickly switch to your Inbox -- press ``ji''
  ;; then, when you want archive some messages, move them to
  ;; the 'All Mail' folder by pressing ``ma''.

  (setq mu4e-maildir-shortcuts
        '(("/Inbox" . ?i)
          ("/Sent" . ?s)
          ("/Trash" . ?t)))

  ;; allow for updating mail using 'U' in the main view:
  (setq mu4e-get-mail-command "offlineimap")

  ;; ;; something about ourselves
  ;; (setq
  ;;    mu4e-compose-signature
  ;;     (concat
  ;;       "Foo X. Bar\n"
  ;;       "http://www.example.com\n"))

  ;; sending mail -- replace USERNAME with your gmail username
  ;; also, make sure the gnutls command line utils are installed
  ;; package 'gnutls-bin' in Debian/Ubuntu

  (require 'smtpmail)
  (setq message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        smtpmail-starttls-credentials '(("smtp.kolabnow.com" 587 nil nil))
        smtpmail-auth-credentials
        '(("smtp.kolabnow.com" 587 "jonas.collberg@mykolab.com" nil))
        smtpmail-default-smtp-server "smtp.kolabnow.com"
        smtpmail-smtp-server "smtp.kolabnow.com"
        smtpmail-smtp-service 587)

  ;; (setq message-send-mail-function 'smtpmail-send-it
  ;;     smtpmail-stream-type 'starttls
  ;;     smtpmail-default-smtp-server "smtp.kolabnow.com"
  ;;     smtpmail-smtp-server "smtp.kolabnow.com"
  ;;     smtpmail-smtp-service 587)

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "Private"
             :enter-func (lambda ()
                           (mu4e-message "Switch to the Private context"))
             ;; leave-func not defined
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches
                              msg :to "jonas.collberg@mykolab.com")))
             :vars '((user-mail-address . "jonas.collberg@mykolab.com"  )
                     ;; (mu4e-compose-signature . "Jonas\n")
                     ))
           ,(make-mu4e-context
             :name "Work"
             :enter-func (lambda () (mu4e-message "Switch to the Work context"))
             ;; leave-fun not defined
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches
                              msg :to "jonas.collberg@orzone.com")))
             :vars '((user-mail-address . "jonas.collberg@orzone.com" )
                     ;; (mu4e-compose-signature . (concat
                     ;;                             "Kind regards,\n"
                     ;;                             user-full-name))
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
