(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'markdown-mode-hook 'autofill-mode)

(add-hook 'markdown-mode-hook
          (lambda ()
            (setq evil-shift-width 4)))

(provide 'init-markdown)
