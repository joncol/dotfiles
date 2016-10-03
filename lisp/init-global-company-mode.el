(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'global-company-mode-hook
          (lambda ()
            (add-to-list 'company-backends 'company-dabbrev-code)

            (setq company-tooltip-limit 20)
            (setq company-idle-delay .3)
            (setq company-echo-delay 0)
            (setq company-begin-commands '(self-insert-command))
            (define-key company-active-map (kbd "j")
              'company-select-next-or-abort)
            (define-key company-active-map (kbd "k")
              'company-select-previous-or-abort)))

(provide 'init-global-company-mode)
