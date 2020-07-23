(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'markdown-mode-hook
          (lambda ()
            (auto-fill-mode)
            (setq evil-shift-width 4)
            (modify-syntax-entry ?- "w") ;; do not treat "_" as a word separator
            (footnote-mode)
            (turn-on-orgtbl)))

(provide 'init-markdown)
