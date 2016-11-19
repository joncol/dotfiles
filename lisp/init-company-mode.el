(add-hook 'after-init-hook 'global-company-mode)

(defun on-off-fci-before-company(command)
  "Fixes the issue where the first item is shown far off to the right."
  (when (string= "show" command)
    (turn-off-fci-mode))
  (when (string= "hide" command)
    (turn-on-fci-mode)))

(advice-add 'company-call-frontends :before #'on-off-fci-before-company)

(add-hook 'company-mode-hook
          (lambda ()
            ;; (add-to-list 'company-backends 'company-dabbrev-code)

            (setq company-tooltip-limit 20)
            (setq company-idle-delay .3)
            (setq company-echo-delay 0)
            (setq company-begin-commands '(self-insert-command))

            (jco/define-bindings company-active-map
                                 '(("j" . company-select-next-or-abort)
                                   ("k" . company-select-previous-or-abort)))))

(provide 'init-company-mode)
