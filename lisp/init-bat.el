(add-hook 'bat-mode-hook
          (lambda ()
            ;; do not treat "_" as a word separator
            (modify-syntax-entry ?_ "w")))

(provide 'init-bat)
