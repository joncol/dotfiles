;;; #init-lisp.el --- Common Lisp config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(require 'init-lisp-common)

(use-package slime
  :after lisp-mode
  :config
  (setq slime-description-autofocus t)
  (add-hook 'lisp-mode-hook
            #'(lambda ()
                (evil-leader/set-key "x s" 'slime)))
  (add-hook 'slime-popup-buffer-mode-hook
            #'(lambda ()
                (evil-motion-state)))
  (add-hook 'slime-repl-mode-hook
            #'(lambda ()
                (evil-normal-state)
                (turn-off-fci-mode))))

(add-hook 'slime-connected-hook
          #'(lambda ()
              (with-selected-window (get-buffer-window (slime-output-buffer t))
                (let ((height (if (or (jco/at-office-p)
                                      (display-graphic-p)) 15 10)))
                  (jco/move-window-to-bottom height)))))

(add-hook 'lisp-mode-hook
          #'(lambda ()
              (init-lisp-common)
              (require 'redshank-loader)
              (with-eval-after-load 'redshank-loader
                (redshank-setup '(lisp-mode-hook
                                  slime-repl-mode-hook) t))
              (diminish 'redshank-mode)
              (redshank-mode)
              (setq-local evil-move-beyond-eol t)
              (modify-syntax-entry ?: "w")
              (modify-syntax-entry ?< "w")
              (modify-syntax-entry ?> "w")
              (modify-syntax-entry ?= "w")
              (setq inferior-lisp-program "sbcl")
              (slime-setup '(slime-asdf slime-company slime-fancy))
              (slime-asdf-init) ;; Required for `slime-load-system'.
              (evil-leader/set-key "x l" #'slime-load-system)
              (slime-company-maybe-enable)
              (bind-key (kbd "M-.") 'slime-edit-definition lisp-mode-map)
              (define-key sldb-mode-map "\C-w" 'evil-window-map)))

(use-package slime-company
  :defer t)

(when (file-exists-p "~/quicklisp/slime-helper.el")
  (load (expand-file-name "~/quicklisp/slime-helper.el")))

(provide 'init-lisp)

;;; init-lisp.el ends here
