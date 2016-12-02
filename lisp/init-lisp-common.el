;;; #init-lisp-common.el --- All Lisp modes -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(defun init-lisp-common ()
  "Common configuration options for all Lisp modes."

  ;; do not treat "-" as a word separator
  (modify-syntax-entry ?- "w")
  (paredit-mode)
  (evil-paredit-mode)
  (diminish 'paredit-mode)
  (use-package redshank
    :demand
    :diminish
    :config
    (redshank-mode)
    (setq redshank-prefix-key "C-c C-r")))

(provide 'init-lisp-common)

;;; init-lisp-common.el ends here
