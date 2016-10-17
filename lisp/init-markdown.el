(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'markdown-mode-hook 'auto-fill-mode)

(add-hook 'markdown-mode-hook
          (lambda ()
            (setq evil-shift-width 4)
            (footnote-mode)))

(provide 'init-markdown)
