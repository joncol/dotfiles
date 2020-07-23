(add-hook 'ecb-activate-hook
          (lambda ()
            (set-face-background 'ecb-default-highlight-face
                                 "midnight blue")))

(provide 'init-ecb)
