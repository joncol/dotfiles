(when (eq system-type 'gnu/linux)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (require 'rtags)
              (when (not (boundp 'cmake-ide-build-dir))
                (defvar cmake-ide-build-dir
                  (concat (projectile-project-root) "../_build")))
              (cmake-ide-setup)

              (eval-after-load "evil"
                '(bind-keys :map evil-normal-state-map
                            ("M-." . nil)
                            ;; ("gf"  . rtags-find-file)
                            ))

              (rtags-enable-standard-keybindings)
              (setq rtags-use-helm t)
              ;; (setq rtags-use-filename-completion nil)

              (push 'company-rtags company-backends)

              (bind-key "M-." 'rtags-find-symbol-at-point))

            ;; Fix color of filenames when using rtags-find-file.
            (add-hook 'minibuffer-inactive-mode-hook
                      (lambda ()
                        (face-remap-add-relative 'font-lock-string-face
                                                 '((:background nil)))
                        (face-remap-add-relative 'font-lock-preprocessor-face
                                                 '((:background nil)))))))

(provide 'init-rtags)
