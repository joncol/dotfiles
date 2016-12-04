;;; #init-lisp-common.el --- All Lisp modes -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package redshank
  :defer t
  :init
  (setq redshank-prefix-key "C-c C-r")
  :diminish redshank-mode)

(use-package paredit
  :defer t
  :diminish paredit-mode)

(use-package evil-paredit
  :defer t)

(use-package evil-lispy
  :commands evil-lispy-mode
  :defer t)

(defun init-lisp-common ()
  "Common configuration options for all Lisp modes."

  ;; do not treat "-" as a word separator
  (modify-syntax-entry ?- "w")

  (paredit-mode)
  (evil-paredit-mode)
  (evil-lispy-mode)
  (redshank-mode))

(provide 'init-lisp-common)

;;; init-lisp-common.el ends here
