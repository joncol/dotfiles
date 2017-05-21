;;; #init-projectile.el --- Projectile config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package projectile
  :init
  (projectile-mode)

  :config
  (when helm-mode
    (define-key projectile-command-map (kbd "s a") #'helm-ag-project-root)
    (setq projectile-completion-system 'helm))

  (when (not (eq system-type 'windows-nt))
    (setq projectile-indexing-method 'native))

  (setq projectile-enable-caching t)

  :diminish projectile-mode)

(use-package helm-projectile
  :if helm-mode
  :init
  (helm-projectile-on))

(provide 'init-projectile)

;;; init-projectile.el ends here
