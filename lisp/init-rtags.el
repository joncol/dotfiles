;;; init-rtags.el --- Setup RTags for C++ completion and flycheck support.

;;; Commentary:

;;; Code:

(when (eq system-type 'gnu/linux)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (require 'rtags)
              (require 'flycheck-rtags)
              (require 'projectile)
              (when (not (boundp 'cmake-ide-build-dir))
                (defvar cmake-ide-build-dir
                  (concat (projectile-project-root) "_build")))
              (cmake-ide-setup)

              (rtags-enable-standard-keybindings)
              (setq rtags-use-helm t)

              (push 'company-rtags company-backends)

              (bind-key "M-." 'rtags-find-symbol-at-point c-mode-base-map)

              (add-hook 'rtags-jump-hook 'evil-set-jump)

              ;; Fix color of filenames when using rtags-find-file.
              (add-hook 'minibuffer-inactive-mode-hook
                        (lambda ()
                          (face-remap-add-relative 'font-lock-string-face
                                                   '((:background nil)))
                          (face-remap-add-relative 'font-lock-preprocessor-face
                                                   '((:background nil)))))

              (dolist (mode '(c-mode c++-mode))
                (evil-leader/set-key-for-mode mode
                  "g ." 'rtags-find-symbol-at-point
                  "g ," 'rtags-find-references-at-point
                  "g v" 'rtags-find-virtuals-at-point
                  "g V" 'rtags-print-enum-value-at-point
                  "g /" 'rtags-find-all-references-at-point
                  "g Y" 'rtags-cycle-overlays-on-screen
                  "g >" 'rtags-find-symbol
                  "g <" 'rtags-find-references
                  "g [" 'rtags-location-stack-back
                  "g ]" 'rtags-location-stack-forward
                  "g C" 'rtags-compile-file
                  "g D" 'rtags-diagnostics
                  "g G" 'rtags-guess-function-at-point
                  "g p" 'rtags-set-current-project
                  "g P" 'rtags-print-dependencies
                  "g e" 'rtags-reparse-file
                  "g E" 'rtags-preprocess-file
                  "g R" 'rtags-rename-symbol
                  "g M" 'rtags-symbol-info
                  "g S" 'rtags-display-summary
                  "g O" 'rtags-goto-offset
                  "g ;" 'rtags-find-file
                  "g F" 'rtags-fixit
                  "g L" 'rtags-copy-and-print-current-location
                  "g X" 'rtags-fix-fixit-at-point
                  "g B" 'rtags-show-rtags-buffer
                  "g I" 'rtags-imenu
                  "g T" 'rtags-taglist
                  "g h" 'rtags-print-class-hierarchy
                  "g a" 'rtags-print-source-arguments)))))

(provide 'init-rtags)

;;; init-rtags.el ends here
