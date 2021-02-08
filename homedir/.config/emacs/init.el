;;; init.el --- Main startup file -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
               (expand-file-name "straight/repos/straight.el/bootstrap.el"
                                 user-emacs-directory))
            (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
          (with-current-buffer
                    (url-retrieve-synchronously
                               "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                                        'silent 'inhibit-cookies)
                          (goto-char (point-max))
                                (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq-default flycheck-emacs-lisp-load-path load-path)

(defvar evil-want-C-i-jump nil)

;; Base initialization
(require 'init-common-functions)
(require 'init-evil)
(require 'init-common)
(require 'init-elfeed)
(require 'init-eshell)
(require 'init-font)
(require 'init-company)
(require 'init-arduino)
(require 'init-hydras)
(require 'init-mu4e)
(require 'init-projectile)
(require 'init-rotate)
(require 'init-shackle)
(require 'init-theme)
(require 'init-modeline)
(require 'init-tab)
(require 'init-utils)
(require 'init-wspace)
(require 'init-yas)
(require 'init-zimpler)

;; Programming mode initialization
(require 'init-clojure)
(require 'init-emacs-lisp)
(require 'init-go)
(require 'init-lang)
(require 'init-lisp)
(require 'init-macros)
(require 'init-org)
(require 'init-rust)

;; Change for different username.
(setq inhibit-startup-echo-area-message "jco")
(setq inhibit-startup-message t)

(provide 'init)

;;; init.el ends here
