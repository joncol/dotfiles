;;; #init-wspace.el --- Whitespace config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(defun jco/no-final-newline ()
  (setq require-final-newline nil)
  (setq mode-require-final-newline nil))

(defun jco/tabs-are-ok ()
  (setq ethan-wspace-errors (remove 'tabs ethan-wspace-errors)))

(use-package ethan-wspace
  :config
  (jco/no-final-newline)
  (global-set-key (kbd "M-<backspace>") 'ethan-wspace-clean-all)
  (global-set-key (kbd "M-S-<backspace>") 'delete-trailing-whitespace)

  (add-hook 'makefile-mode-hook 'jco/tabs-are-ok)

  (add-hook 'sml-mode-hook 'jco/no-final-newline t)
  (add-hook 'fsharp-mode-hook 'jco/no-final-newline t)
  (add-hook 'ruby-mode-hook 'jco/no-final-newline t)
  (defadvice ruby-mode-variables (after reset-final-newline activate compile)
    "Reset final-newline that ruby-mode enforces but conflicts with
ethan-wspace."
    (jco/no-final-newline)))

(use-package whitespace
  :config
  (setq-default whitespace-style '(face tabs trailing
                                        space-before-tab indentation
                                        empty space-after-tab tab-mark))
  (add-hook 'after-init-hook
            (lambda ()
              (set-face-background 'whitespace-trailing "#82589f"))))

(global-set-key (kbd "C-M-S-<backspace>") 'jco/tighten-braces)
(evil-leader/set-key "s b" 'jco/tighten-braces)


(provide 'init-wspace)

;;; init-wspace.el ends here
