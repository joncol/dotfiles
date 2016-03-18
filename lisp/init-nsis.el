(add-hook 'nsis-mode-hook
          (lambda ()
            (fci-mode)
            (modify-syntax-entry ?_ "w") ;; do not treat "_" as a word separator
            (no-final-newline)))

(provide 'init-nsis)
