(setq lua-indent-level 2)

(add-hook 'lua-mode-hook
          (lambda ()
            (setq evil-shift-width 2)))

(provide 'init-lua)
