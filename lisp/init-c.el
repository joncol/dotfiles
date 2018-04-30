;;; #init-c.el --- C config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(add-hook 'c-mode-hook
          (lambda ()
            (setq compile-command
                  (concat "cd " (projectile-project-root)
                          "debug ;and make -j4"))
            (bind-key "C-c C-c" #'compile c-mode-map)
            (c-set-offset 'label '-)
            (setq comment-start "//"
                  comment-end "")))

(provide 'init-c)

;;; init-c.el ends here
