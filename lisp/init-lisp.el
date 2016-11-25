(require 'init-lisp-common)

(add-hook 'lisp-mode-hook
          (lambda ()
            (init-lisp-common)
            (setq inferior-lisp-program "sbcl")
            (bind-key (kbd "M-.") 'slime-edit-definition lisp-mode-map)))

(when (file-exists-p "~/quicklisp/slime-helper.el")
  (load (expand-file-name "~/quicklisp/slime-helper.el")))

(provide 'init-lisp)
