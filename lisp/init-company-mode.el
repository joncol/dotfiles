(add-hook 'after-init-hook 'global-company-mode)

(add-hook 'company-mode-hook
          (lambda ()
            ;; (add-to-list 'company-backends 'company-dabbrev-code)

            (setq company-tooltip-limit 20)
            (setq company-idle-delay .3)
            (setq company-echo-delay 0)
            (setq company-begin-commands '(self-insert-command))

            (jco/define-bindings company-active-map
                                 '(("j" . company-select-next-or-abort)
                                   ("k" . company-select-previo-or-abort)))))

(provide 'init-company-mode)
