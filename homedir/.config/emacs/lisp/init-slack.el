;;; #init-slack.el --- Slack config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(defun jco/slack-client-id ()
  "Return Slack client ID."
  (if (file-exists-p "~/.emacs-slack")
    (let ((lines (jco/read-lines "~/.emacs-slack")))
      (car lines))
    ""))

(defun jco/slack-client-secret ()
  "Return Slack client secret."
  (if (file-exists-p "~/.emacs-slack")
    (let ((lines (jco/read-lines "~/.emacs-slack")))
      (cadr lines))
    ""))

(defun jco/slack-client-token ()
  "Return Slack client token."
  (if (file-exists-p "~/.emacs-slack")
    (let ((lines (jco/read-lines "~/.emacs-slack")))
      (caddr lines))
    ""))

(use-package slack
  :commands (slack-start)

  :init
  (setq slack-buffer-emojify t)
  (setq slack-prefer-current-team t)

  :config
  (slack-register-team
   :name "TestChat"
   :default t
   :client-id (jco/slack-client-id)
   :client-secret (jco/slack-client-secret)
   :token (jco/slack-client-token)
   :subscribed-channels '())

  (evil-define-key 'normal slack-mode-map
    "\C-n" 'slack-buffer-goto-next-message
    "\C-p" 'slack-buffer-goto-prev-message))

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

(provide 'init-slack)

;;; init-slack.el ends here
