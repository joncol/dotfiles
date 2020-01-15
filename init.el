;;; init.el --- Main startup file -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook (lambda ()
                             ;; Restore after startup.
                             (setq gc-cons-threshold 800000)))
(setq package-archives
      '(("elpa"  . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org"   . "http://orgmode.org/elpa/")))

(package-initialize) ;; Don't delete this. It will be readded automatically.

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(let ((bootstrap-file (concat user-emacs-directory
                              "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         (concat "https://raw.githubusercontent.com/raxod502/"
                 "straight.el/develop/install.el")
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq-default flycheck-emacs-lisp-load-path load-path)

(defvar evil-want-C-i-jump nil)

;; Base initialization
(when (version< emacs-version "27")
  (require 'init-security))

(require 'init-common-functions)
(require 'init-evil)
(require 'init-common)
(require 'init-elfeed)
(require 'init-eshell)
(require 'init-fci)
(require 'init-font)
(require 'init-company)
(require 'init-arduino)
(require 'init-hydras)
(require 'init-ido)
(require 'init-mu4e)
(require 'init-neotree)
(require 'init-projectile)
(require 'init-rotate)
(require 'init-tab)
(require 'init-theme)
(require 'init-modeline)
(require 'init-utils)
(require 'init-wspace)
(require 'init-yas)

;; Programming mode initialization
(require 'init-common-programming)
(require 'init-bat)
(require 'init-flymake)
(require 'init-c)
(require 'init-cc)
(require 'init-clojure)
(require 'init-cmake)
(require 'init-cmake-ide)
(require 'init-conf)
(require 'init-cpp)
(require 'init-csharp)
(require 'init-ecb)
(require 'init-emacs-lisp)
(require 'init-go)
(require 'init-haskell)
(require 'init-lang)
(require 'init-lisp)
(require 'init-lua)
(require 'init-macros)
(require 'init-markdown)
(require 'init-nsis)
(require 'init-nxml)
(require 'init-org)
(require 'init-python)
(require 'init-qml)
(require 'init-racket)
(require 'init-ruby)
(require 'init-scheme)
(require 'init-semantic)
(require 'init-standard-ml)

;; Change for different username.
(setq inhibit-startup-echo-area-message "jco")
(setq inhibit-startup-message t)

(provide 'init)

;;; init.el ends here
