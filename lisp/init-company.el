;;; #init-company.el --- Company config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)

  :config
  (advice-add 'company-call-frontends :before
              #'jco/turn-off-fci-during-company-complete)
  (jco/define-bindings company-active-map
                       '(("C-j" . company-select-next-or-abort)
                         ("C-k" . company-select-previous-or-abort)))

  ;; (setq tab-always-indent 'complete)
  (add-to-list 'completion-styles 'initials t)
  (setq company-dabbrev-ignore-case 'keep-prefix)
  (setq company-dabbrev-code-ignore-case nil)
  (setq company-dabbrev-downcase nil)

  (setq company-tooltip-limit 20)
  (setq company-idle-delay .3)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command))

  :diminish)

(defun jco/turn-off-fci-during-company-complete(command)
  "Fixes the issue where the first item is shown far off to the right."
  (when (string= "show" command)
    (turn-off-fci-mode))
  (when (string= "hide" command)
    (turn-on-fci-mode)))

(provide 'init-company)

;;; init-company.el ends here
