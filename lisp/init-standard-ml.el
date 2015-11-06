(setq sml-indent-level 2)
(setq sml-program-name "/usr/local/bin/sml")

(add-hook 'sml-mode-hook
          (lambda ()
            (setq evil-shift-width 2)))

(provide 'init-standard-ml)
