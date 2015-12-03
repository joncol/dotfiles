(defun init-lisp-common ()
  (smartparens-mode -1)
  (paredit-mode)
  (evil-paredit-mode)
  (setq redshank-prefix-key "C-c C-r")
  (redshank-mode))

(provide 'init-lisp-common)
