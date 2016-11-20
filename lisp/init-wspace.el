(require 'ethan-wspace)

(defun no-final-newline ()
  (setq require-final-newline nil)
  (setq mode-require-final-newline nil))

(no-final-newline)

(eval-after-load "ethan-wspace" '(diminish 'ethan-wspace-mode))

(defun tabs-are-ok ()
  (setq ethan-wspace-errors (remove 'tabs ethan-wspace-errors)))

(add-hook 'makefile-mode-hook 'tabs-are-ok)

(add-hook 'sml-mode-hook 'no-final-newline t)
(add-hook 'fsharp-mode-hook 'no-final-newline t)
(add-hook 'ruby-mode-hook 'no-final-newline t)

(defadvice ruby-mode-variables (after reset-final-newline activate compile)
  "Reset final-newline that ruby-mode enforces but conflicts with ethan-wspace."
  (no-final-newline))

(require 'whitespace)

(diminish 'whitespace-mode)

(setq-default whitespace-style '(face tabs trailing
                                      space-before-tab indentation
                                      empty space-after-tab tab-mark))
(set-face-background 'whitespace-trailing "#ff0000")

(global-set-key (kbd "M-<backspace>") 'ethan-wspace-clean-all)

(provide 'init-wspace)
