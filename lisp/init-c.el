;;; #init-c.el --- C config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(add-hook 'c-mode-hook
          (lambda ()
            (setq compile-command
                  (concat "cd " (projectile-project-root)
                          "debug ;and make -j4"))
            (jco/define-bindings c-mode-map
                                 '(("<f6>" . compile)
                                   ("C-c C-k" . compile)))
            (c-set-offset 'label '-)
            (setq comment-start "//"
                  comment-end "")))

(provide 'init-c)

;;; init-c.el ends here
