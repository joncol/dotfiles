;;; #init-projectile.el --- Projectile config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package projectile
  :init
  (projectile-mode)

  :config
  (when (not (eq system-type 'windows-nt))
    (setq projectile-indexing-method 'native))
  (setq projectile-enable-caching t)
  ;; (setq projectile-switch-project-action #'projectile-commander)
  (evil-leader/set-key "x p" #'projectile-commander)
  (evil-leader/set-key "x f" #'counsel-projectile-find-file)
  (evil-leader/set-key "x a" #'counsel-projectile-ag)
  (def-projectile-commander-method ?a
    "Ag."
    (counsel-projectile-ag))
  (def-projectile-commander-method ?F
    "Git fetch."
    (magit-status)
    (if (fboundp 'magit-fetch-from-upstream)
        (call-interactively #'magit-fetch-from-upstream)
      (call-interactively #'magit-fetch-current)))
  (def-projectile-commander-method ?j
    "Jack-in."
    (let* ((opts (projectile-current-project-files))
           (file (ido-completing-read
                  "Find file: "
                  opts
                  nil nil nil nil
                  (car (cl-member-if
                        (lambda (f)
                          (string-match "core\\.clj\\'" f))
                        opts)))))
      (find-file (expand-file-name
                  file (projectile-project-root)))
      (run-hooks 'projectile-find-file-hook)
      (cider-jack-in)))

  :diminish projectile-mode)

(provide 'init-projectile)

;;; init-projectile.el ends here
