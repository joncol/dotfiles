(add-hook 'csharp-mode-hook
          (lambda ()
            (electric-pair-mode 0)
            (add-to-list 'company-backends 'company-omnisharp)
            (c-set-style "c#")
            (omnisharp-mode)
            (flycheck-mode)
            (local-set-key (kbd "M-g") 'omnisharp-go-to-definition)))

(provide 'init-csharp)
