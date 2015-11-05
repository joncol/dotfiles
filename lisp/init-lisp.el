(add-hook 'lisp-mode-hook
          '(lambda ()
             (paredit-mode)
             (evil-paredit-mode)))

(when (file-exists-p "~/quicklisp/slime-helper.el")
  (load (expand-file-name "~/quicklisp/slime-helper.el")))
(setq inferior-lisp-program "sbcl")
(jco/define-bindings evil-normal-state-map '(("M-." . nil)))
(jco/define-bindings lisp-mode-map '(("M-." . slime-edit-definition)))

(provide 'init-lisp)
