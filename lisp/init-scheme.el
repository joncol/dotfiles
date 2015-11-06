(add-hook 'scheme-mode-hook
          (lambda ()
            (paredit-mode)
            (evil-paredit-mode)))

(setq scheme-mit-dialect nil)

(provide 'init-scheme)
