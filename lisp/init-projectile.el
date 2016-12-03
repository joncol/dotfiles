;;; #init-projectile.el --- Projectile config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package projectile
  :init
  (projectile-mode)

  :config
  (define-key projectile-command-map (kbd "s a") #'helm-ag-project-root)

  (when (not (eq system-type 'windows-nt))
    (setq projectile-indexing-method 'native))
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'helm)

  :diminish projectile-mode)

(use-package helm-projectile
  :init
  (helm-projectile-on))

(provide 'init-projectile)

;;; init-projectile.el ends here
