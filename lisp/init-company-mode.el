(add-hook 'after-init-hook 'global-company-mode)

(defun turn-off-fci-during-company-complete(command)
  "Fixes the issue where the first item is shown far off to the right."
  (when (string= "show" command)
    (turn-off-fci-mode))
  (when (string= "hide" command)
    (turn-on-fci-mode)))

(advice-add 'company-call-frontends :before
            #'turn-off-fci-during-company-complete)

(add-hook 'company-mode-hook
          (lambda ()
            ;; (add-to-list 'company-backends 'company-dabbrev-code)

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
            (setq company-begin-commands '(self-insert-command))))

(provide 'init-company-mode)
