(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (paredit-mode)
             (evil-paredit-mode)))

(provide 'init-emacs-lisp)
