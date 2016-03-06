(require 'cc-mode)

(add-hook 'c-mode-hook
          (lambda ()
            (setq compile-command
                  (if (jco/at-office-p)
                    (concat "cd " (projectile-project-root)
                            "debug && make -j4")
                    (concat "cd " (projectile-project-root)
                            "debug ;and make -j4")))
            (jco/define-bindings c-mode-map
                                 '(("<f6>" . compile)))
            (c-set-offset 'label '-)
            (setq comment-start "//"
                  comment-end "")))

(provide 'init-c)
