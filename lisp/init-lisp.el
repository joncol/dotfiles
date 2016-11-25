(require 'init-lisp-common)

(add-hook 'lisp-mode-hook
          (lambda ()
            (init-lisp-common)
            (eval-after-load "evil"
              '(jco/define-bindings evil-normal-state-map '(("M-." . nil))))
            (setq inferior-lisp-program "sbcl")
            (bind-key (kbd "M-.") 'slime-edit-definition lisp-mode-map)))

(when (file-exists-p "~/quicklisp/slime-helper.el")
  (load (expand-file-name "~/quicklisp/slime-helper.el")))

(provide 'init-lisp)
