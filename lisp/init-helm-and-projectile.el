;;; #init-helm-and-projectile.el --- Helm and projectile config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-m") 'helm-semantic-or-imenu)

(require 'helm-ag)
(require 'helm-buffers)
(require 'helm-files)
(require 'helm-gtags)
(require 'helm-net)

(when (executable-find "curl")
  (setq helm-net-prefer-curl t))

(setq helm-split-window-in-side-p           t
      helm-buffers-fuzzy-matching           t
      helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-scroll-amount                    8
      helm-ff-file-name-history-use-recentf t)

(setq helm-gtags-ignore-case t
      helm-gtags-auto-update t
      helm-gtags-use-input-at-cursor t
      helm-gtags-pulse-at-cursor t
      helm-gtags-suggested-key-mapping t
      helm-ag-base-command "ag --nocolor --nogroup --line-numbers --smart-case"
      helm-ag-insert-at-point 'word
      helm-gtags-path-style 'absolute)

(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(eval-after-load "helm-gtags" '(diminish 'helm-gtags-mode))

(helm-mode)
(diminish 'helm-mode)

(require 'helm-descbinds)
(helm-descbinds-mode)

;;; projectile
(require 'projectile)
(projectile-mode)
(diminish 'projectile-mode)
(when (not (eq system-type 'windows-nt))
  (setq projectile-indexing-method 'native))
(setq projectile-enable-caching t)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(provide 'init-helm-and-projectile)

;;; init-helm-and-projectile.el ends here
