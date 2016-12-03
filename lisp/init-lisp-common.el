;;; #init-lisp-common.el --- All Lisp modes -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(defun init-lisp-common ()
  "Common configuration options for all Lisp modes."

  ;; do not treat "-" as a word separator
  (modify-syntax-entry ?- "w")
  (use-package paredit
    :init
    (paredit-mode)
    :diminish)

  (use-package evil-paredit
    :init
    (evil-paredit-mode))

  (use-package redshank
    :demand t
    :init
    (setq redshank-prefix-key "C-c C-r")
    :config
    (redshank-mode)
    :diminish redshank-mode))

(provide 'init-lisp-common)

;;; init-lisp-common.el ends here
