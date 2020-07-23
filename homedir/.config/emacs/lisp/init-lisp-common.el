;;; #init-lisp-common.el --- All Lisp modes -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(defun jco/lisp-comment-dwim ()
  "Comments Lisp sexps smartly."
  (interactive)
  (if (and (not (hlt-nonempty-region-p))
           (member (char-after) '(?\( ?{ ?\[)))
      (progn (mark-sexp)
             (comment-dwim nil))
    (call-interactively #'evilnc-comment-or-uncomment-lines)))

(defun init-lisp-common ()
  "Common configuration options for all Lisp modes."
  (aggressive-indent-mode)
  (setq evil-shift-width 2)
  (define-key lisp-mode-shared-map (kbd "M-;") #'jco/lisp-comment-dwim)
  ;; do not treat "-" as a word separator
  (modify-syntax-entry ?- "w")
  (smartparens-strict-mode))

(provide 'init-lisp-common)

;;; init-lisp-common.el ends here
