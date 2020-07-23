(add-to-list 'auto-mode-alist '("/\\.[^/]*rc" . conf-mode) t)

(add-hook 'conf-unix-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")))

(provide 'init-conf)
