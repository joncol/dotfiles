;;; #init-common.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

;;; Avoid the empty (custom-set-faces) at end of init.el.
(setq custom-file (expand-file-name (concat user-emacs-directory "custom.el")))

(setq ad-redefinition-action 'accept)

(tool-bar-mode -1)
(global-auto-revert-mode)
(global-font-lock-mode)
(show-paren-mode)
(global-hl-line-mode)
(global-nlinum-mode)

(use-package elec-pair
  :init
  (electric-pair-mode)
  :config
  (setq electric-pair-preserve-balance nil))

(modify-syntax-entry ?_ "w") ;; do not treat "_" as a word separator

(defalias 'yes-or-no-p 'y-or-n-p)
(setq auto-save-default nil)

(require 'server)
(when (not (server-running-p))
  (server-start))

(edit-server-start)

(setq inhibit-startup-message t)
(setq sentence-end-double-space nil)
(setq ring-bell-function 'ignore)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(electric-indent-mode)
(global-set-key (kbd "RET")
                (lambda ()
                  (interactive)
                  (delete-trailing-whitespace (line-beginning-position)
                                              (line-end-position))
                  (newline-and-indent)))

(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq scroll-margin 4)

(load-library "iso-transl")
(setq system-time-locale "C")

(require 'time)
(setq display-time-string-forms '(24-hours ":" minutes))

(display-time-mode)

(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-x a r") 'align-regexp)
(defadvice align-regexp (around align-regexp-with-spaces activate compile)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))

(use-package info
  :config
  (bind-keys :map Info-mode-map
             ("<tab>"     . Info-next-reference)
             ("<backtab>" . Info-prev-reference)))

(use-package help-mode
  :config
  (bind-keys :map help-mode-map
             ("<tab>"     . forward-button)
             ("<backtab>" . backward-button)))

(global-set-key (kbd "C-c C-b") 'help-go-back)
(global-set-key (kbd "C-c C-f") 'help-go-forward)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(let ((my-bin-path (expand-file-name "~/.local/bin")))
  (setenv "PATH" (concat (getenv "PATH") ":" my-bin-path))
  (add-to-list 'exec-path my-bin-path t))

(use-package cider
  :config
  (setq cider-show-error-buffer 'nil))

(use-package ecb
  :config
  (setq ecb-tip-of-the-day nil))

(setq large-file-warning-threshold nil)
(setq safe-local-variable-values
      '((org-archive-location . "::* Archived Tasks")))

(use-package recentf
  :config
  (recentf-mode)
  (setq recentf-max-menu-items 25))

(use-package fortune
  :config
  (setq fortune-dir "/usr/share/games/fortunes")
  (setq fortune-file "/usr/share/games/fortunes"))

(use-package guide-key
  :diminish guide-key-mode
  :config
  (guide-key-mode)
  ;; (setq guide-key/popup-window-position "right")
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c C-r")))

(put 'erase-buffer 'disabled nil)

(windmove-default-keybindings)

(setq compilation-scroll-output t)

(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode))

(use-package anzu
  :diminish anzu-mode
  :config
  (global-anzu-mode))

(use-package string-inflection
  :config
  (global-unset-key (kbd "C-q"))
  (global-set-key (kbd "C-q C-u") 'string-inflection-all-cycle))

(ace-link-setup-default "f")

(require 'fuzzy)
(turn-on-fuzzy-isearch)

(require 'qmake-mode)
(require 'iedit)

(require 'diminish)
(eval-after-load "company" '(diminish 'company-mode))

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t))

(global-set-key (kbd "C-x o") 'ace-window)

(eval-after-load "info" '(require 'info+))

(diminish 'abbrev-mode)

(use-package ace-isearch
  :diminish ace-isearch-mode
  :config
  (global-ace-isearch-mode))

(use-package ace-jump-helm-line-mode
  :diminish ace-jump-helm-line-mode
  :bind (:map helm-map
              ("M-f" . ace-jump-helm-line)))

(use-package avy
  :config
  (eval-after-load "evil"
    '(progn (evil-leader/set-key "f" 'evil-avy-goto-char)
            (evil-leader/set-key "#" 'evil-avy-goto-line)
            (evil-leader/set-key "F" 'evil-avy-goto-word-or-subword-1)
            (avy-setup-default)))

  (setq avy-case-fold-search nil))

(use-package sx-question-mode
  :config
  (bind-keys :map sx-question-mode-map
             ("j" . scroll-up-line)
             ("k" . scroll-down-line)))

(add-hook 'after-init-hook #'global-flycheck-mode)
(with-eval-after-load 'flycheck
  (diminish 'flycheck-mode)
  (flycheck-pos-tip-mode))

(require 'bookmark+)

(require 'dired+)

(use-package desktop
  :config
  (push ".*" desktop-clear-preserve-buffers))

(use-package unkillable-scratch
  :init
  (unkillable-scratch))

(require 'magit-mode)

(use-package magit
  :config
  ;; Needed for success status message to be shown.
  (setq magit-auto-revert-mode nil)

  (setq magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-v1))

(use-package outline
  :init
  (if (version< emacs-version "25.1")
      (add-hook 'ediff-prepare-buffer-hook #'show-all)
    (add-hook 'ediff-prepare-buffer-hook #'outline-show-all)))

(put 'narrow-to-region 'disabled nil)

(provide 'init-common)

;;; init-common.el ends here
