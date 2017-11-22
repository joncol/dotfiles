;;; #init-lisp.el --- Common Lisp config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(require 'init-lisp-common)

(use-package slime
  :after lisp-mode
  :config
  (setq slime-description-autofocus t)
  (add-hook 'slime-popup-buffer-mode-hook
            #'(lambda ()
                (evil-motion-state)))
  (add-hook 'slime-repl-mode-hook
            #'(lambda ()
                (turn-off-fci-mode))))

(add-hook 'lisp-mode-hook
          #'(lambda ()
              (init-lisp-common)
              (setq-local evil-move-beyond-eol t)
              (setq inferior-lisp-program "sbcl")
              (setq slime-contribs '(slime-fancy))
              (bind-key (kbd "M-.") 'slime-edit-definition lisp-mode-map)))

(when (file-exists-p "~/quicklisp/slime-helper.el")
  (load (expand-file-name "~/quicklisp/slime-helper.el")))

(provide 'init-lisp)

;;; init-lisp.el ends here
