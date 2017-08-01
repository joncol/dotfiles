;;; #init-lisp-common.el --- All Lisp modes -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package evil-lispy
  :disabled t
  :commands evil-lispy-mode
  :defer t)

(use-package redshank
  :disabled t
  :defer t
  :init
  (setq redshank-prefix-key "C-c C-r")
  :diminish redshank-mode)

(defun init-lisp-common ()
  "Common configuration options for all Lisp modes."

  ;; do not treat "-" as a word separator
  (modify-syntax-entry ?- "w")

  (smartparens-strict-mode))

(provide 'init-lisp-common)

;;; init-lisp-common.el ends here
