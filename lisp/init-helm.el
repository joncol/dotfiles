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
    (evil-leader/set-key "r" 'helm-resume)
    (evil-leader/set-key "h g" 'helm-google-suggest))

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
        helm-gtags-path-style 'absolute)

  :diminish helm-mode)

(use-package helm-ag
  :config
  (setq helm-ag-base-command
        "ag --nocolor --nogroup --line-numbers --smart-case")
  (setq helm-ag-insert-at-point 'word))

(use-package helm-chrome
  :init
  (evil-leader/set-key "h c" 'helm-chrome-bookmarks))

(use-package helm-descbinds
  :init
  (helm-descbinds-mode))

(use-package helm-flx
  :init
  (helm-flx-mode))

(use-package helm-fuzzier
  :disabled t
  :init
  (helm-fuzzier-mode))

(use-package helm-gtags
  :config
  (add-hook 'dired-mode-hook 'helm-gtags-mode)
  (add-hook 'eshell-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)

  :diminish helm-gtags-mode)

(use-package helm-swoop
  :config
  (global-set-key (kbd "M-i") 'helm-swoop)
  (evil-leader/set-key "s" 'helm-swoop)
  (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
  (global-set-key (kbd "C-M-i") 'helm-multi-swoop)
  ;; (evil-leader/set-key "m s" 'helm-multi-swoop)
  (global-set-key (kbd "C-M-S-i") 'helm-multi-swoop-all)
  ;; (evil-leader/set-key "m S" 'helm-multi-swoop-all)

  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

  (define-key helm-swoop-map
    (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

  (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
  (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

  (setq helm-multi-swoop-edit-save t)
  (setq helm-swoop-split-with-multiple-windows nil)
  (setq helm-swoop-split-direction 'split-window-vertically)
  (setq helm-swoop-speed-or-color t)
  (setq helm-swoop-move-to-line-cycle t)
  (setq helm-swoop-use-line-number-face t)

  (setq helm-multi-swoop-ignore-buffers-match
        (concat helm-multi-swoop-ignore-buffers-match "\\|TAGS")))

(provide 'init-helm)

;;; init-helm.el ends here
