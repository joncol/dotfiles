(defun init-lisp-common ()
  (paredit-mode)
  (evil-paredit-mode)
  (setq redshank-prefix-key "C-c C-r")
  (redshank-mode))

(provide 'init-lisp-common)
