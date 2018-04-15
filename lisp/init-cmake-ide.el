;;; init-cmake-ide.el --- Setup cmake-ide -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package cmake-ide
  :if (eq system-type 'gnu/linux)
  :defer t)

(use-package irony
  :if (eq system-type 'gnu/linux)
  :defer t)

(use-package company-irony
  :if (eq system-type 'gnu/linux)
  :defer t)

(defun jco/init-rtags ()
  "Initialize rtags."
  (when (derived-mode-p 'c-mode 'c++-mode)
    (require 'flycheck-rtags) ;; Needed to make flycheck work.
    (setq rtags-completions-enabled t)
    (setq rtags-autostart-diagnostics t)
    (rtags-diagnostics)

    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-irony))

    (bind-key "M-." 'rtags-find-symbol-at-point c-mode-base-map)
    (bind-key "M-," 'rtags-location-stack-back c-mode-base-map)

    (add-hook 'rtags-jump-hook 'evil-set-jump)

    ;; Fix color of filenames when using rtags-find-file.
    (add-hook 'minibuffer-inactive-mode-hook
              (lambda ()
                (face-remap-add-relative 'font-lock-string-face
                                         '((:background nil)))
                (face-remap-add-relative 'font-lock-preprocessor-face
                                         '((:background nil)))))

    (set-face-background 'rtags-argument-face "#8cc4ff")

    ;; Would be nice to be able to use mode-specific bindings here,
    ;; but I didn't get this to work using
    ;; evil-leader/set-key-for-mode.
    (evil-leader/set-key
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
      "g c" 'rtags-compile-file
      "g C" 'rtags-recompile-file
      "g D" 'rtags-diagnostics
      "g G" 'rtags-guess-function-at-point
      "g p" 'rtags-set-current-project
      "g P" 'rtags-print-dependencies
      "g e" 'rtags-reparse-file
      "g E" 'rtags-preprocess-file
      "g r" 'rtags-rename-symbol
      "g m" 'rtags-make-member
      "g M" 'rtags-symbol-info
      "g S" 'rtags-display-summary
      "g O" 'rtags-goto-offset
      "g ;" 'rtags-find-file
      "g f" 'rtags-fix-fixit-at-point
      "g L" 'rtags-copy-and-print-current-location
      "g B" 'rtags-show-rtags-buffer
      "g I" 'rtags-imenu
      "g T" 'rtags-taglist
      "g h" 'rtags-print-class-hierarchy
      "g a" 'rtags-print-source-arguments)))

(use-package rtags
  :defer t
  :config
  (jco/init-rtags))

(when (eq system-type 'gnu/linux)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode)
                (require 'rtags)
                (when (projectile-project-p)
                  (setq cmake-ide-build-dir
                        (concat (projectile-project-root) "_build")))
                (cmake-ide-setup)
                (setq cmake-ide-header-search-other-file nil)
                (setq cmake-ide-flags-c++ (append '("-std=c++14")))
                (global-set-key (kbd "C-c m") 'cmake-ide-run-cmake)
                (irony-mode)))))

(provide 'init-cmake-ide)

;;; init-cmake-ide.el ends here
