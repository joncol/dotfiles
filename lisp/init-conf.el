(add-hook 'conf-unix-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")))

(provide 'init-conf)
