;;; #init-helm.el --- Helm config -*- lexical-binding: t; -*-
;;; Commentary:

;;

;;; Code:

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(use-package helm
  :init
  (helm-mode)

  :config
  (require 'helm-buffers)
  (require 'helm-files)
  (require 'helm-net)

  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-m") 'helm-semantic-or-imenu)
  (when (executable-find "curl")
    (setq helm-net-prefer-curl t))

  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-r") 'helm-recentf)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)

  (with-eval-after-load 'evil-leader
    (evil-leader/set-key "b" 'helm-buffers-list)
    (evil-leader/set-key "r" 'helm-resume))

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

  :diminish helm-mode)

(use-package helm-ag)

(use-package helm-gtags
  :config
  (add-hook 'dired-mode-hook 'helm-gtags-mode)
  (add-hook 'eshell-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)

  :diminish helm-gtags-mode)

(use-package helm-descbinds
  :init
  (helm-descbinds-mode))

(use-package helm-chrome
  :init
  (evil-leader/set-key "h c" 'helm-chrome-bookmarks))

(provide 'init-helm)

;;; init-helm.el ends here
