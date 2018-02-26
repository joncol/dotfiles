(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'markdown-mode-hook
          (lambda ()
            (auto-fill-mode)
            (setq evil-shift-width 4)
            (footnote-mode)
            (turn-on-orgtbl)))

(provide 'init-markdown)
