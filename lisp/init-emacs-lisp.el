;;; #init-emacs-lisp.el --- Emacs Lisp configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (init-lisp-common)))

(provide 'init-emacs-lisp)

;;; init-emacs-lisp.el ends here
