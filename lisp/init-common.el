;;; #init-common.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

;; (require 'use-package)
;; (setq use-package-verbose t)

;;; Avoid the empty (custom-set-faces) at end of init.el.
(setq custom-file (expand-file-name (concat user-emacs-directory "custom.el")))

(setq ad-redefinition-action 'accept)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(global-auto-revert-mode)
(global-font-lock-mode)
(show-paren-mode)
(global-hl-line-mode)
(global-whitespace-mode)
(diminish 'global-whitespace-mode)
(winner-mode)

(when (eq system-type 'gnu/linux)
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "google-chrome-stable"))

(modify-syntax-entry ?_ "w") ;; do not treat "_" as a word separator

(defalias 'yes-or-no-p 'y-or-n-p)
(setq auto-save-default nil)
(setq make-backup-files nil)

(use-package ace-isearch
  :disabled t
  :diminish ace-isearch-mode
  :config
  (global-ace-isearch-mode))

(use-package ace-jump-helm-line
  :diminish ace-jump-helm-line-mode
  :bind (:map helm-map
              ("M-f" . ace-jump-helm-line)))

(use-package ace-link
  :init
  (ace-link-setup-default "f"))

(use-package ace-window
  :init
  (global-set-key [remap other-window] 'ace-window)
  ;; (custom-set-faces
  ;;  '(aw-leading-char-face
  ;;    ((t (:inherit avy-lead-face :height 1.5)))))
  )

(use-package ahk-mode)

(use-package anzu
  :diminish anzu-mode
  :config
  (global-anzu-mode))

(use-package auto-yasnippet
  :config
  (evil-leader/set-key "y c" #'aya-create)
  (evil-leader/set-key "y x" #'aya-expand))

(use-package avy
  :config
  (when jco/use-colemak
    (setq avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o)))

  (evil-leader/set-key "f" 'evil-avy-goto-char)
  (evil-leader/set-key "#" 'evil-avy-goto-line)
  (evil-leader/set-key "F" 'evil-avy-goto-word-or-subword-1)
  (evil-leader/set-key "/" 'avy-goto-char-timer)
  (evil-declare-not-repeat 'avy-goto-char-timer)
  (avy-setup-default)

  (setq avy-case-fold-search nil))

(use-package bookmark+ :disabled t)

(use-package buffer-move
  :config
  (global-set-key (kbd "<C-S-up>") 'buf-move-up)
  (global-set-key (kbd "<C-S-down>") 'buf-move-down)
  (global-set-key (kbd "<C-S-left>") 'buf-move-left)
  (global-set-key (kbd "<C-S-right>") 'buf-move-right))

(use-package cider
  :config
  (setq cider-show-error-buffer 'nil))

(use-package desktop
  :config
  (push ".*" desktop-clear-preserve-buffers))

(use-package dired+ :disabled t)

(use-package counsel
  :after ivy

  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-x C-r" . counsel-recentf)
  ("C-c p s a" . counsel-projectile-ag)

  :config
  (define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done)
  (when (eq system-type 'windows-nt)
    (setq-default counsel-ag-base-command
                  "ag --vimgrep --nocolor --nogroup %s"))
  )

(use-package counsel-projectile
  :init
  (counsel-projectile-on))

(use-package dashboard
  :diminish dashboard-mode

  :config
  (setq dashboard-items '((recents . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)))

  (setq dashboard-banner-logo-title "Welcome to Emacs")
  (setq dashboard-startup-banner 'logo)
  (dashboard-setup-startup-hook))

(use-package ecb
  :config
  (setq ecb-tip-of-the-day nil))

(use-package edit-server
  :init
  (edit-server-start))

(use-package elec-pair
  :init
  (electric-pair-mode)
  :config
  ;; (setq electric-pair-preserve-balance nil)
  (setq electric-pair-skip-whitespace nil)
  (setq electric-pair-delete-adjacent-pairs nil))

(use-package esup)

(use-package evil-magit)

(use-package evil-numbers
  :bind (("C-c +" . evil-numbers/inc-at-pt)
         ("C-c -" . evil-numbers/dec-at-pt)))

(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package f)

(use-package fireplace)

(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq flycheck-pos-tip-timeout 0)
  :diminish 'flycheck-mode)

(use-package flycheck-pos-tip
  :init
  (flycheck-pos-tip-mode))

(use-package fortune
  :config
  (setq fortune-dir "/usr/share/games/fortunes")
  (setq fortune-file "/usr/share/games/fortunes"))

(use-package fuzzy
  :config
  (turn-on-fuzzy-isearch))

(use-package nlinum
  :init
  (nlinum-mode))

(use-package git-gutter+
  :if (not (eq system-type 'windows-nt))
  :init
  (global-git-gutter+-mode))

(use-package git-gutter-fringe+
  :if (not (eq system-type 'windows-nt))
  :after nlinum)

(use-package glsl-mode)

(use-package google-this)

(use-package guide-key
  :diminish guide-key-mode
  :config
  (guide-key-mode)
  ;; (setq guide-key/popup-window-position "right")
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c C-r")))

(use-package imenu-anywhere
  :config
  (global-set-key (kbd "M-m") #'imenu-anywhere))

(global-set-key (kbd "M-M") #'imenu)

(use-package ivy
  :diminish ivy-mode

  :bind
  ("C-s" . swiper)
  ("C-x C-b" . ivy-switch-buffer)

  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (evil-leader/set-key "b" 'ivy-switch-buffer)
  (evil-leader/set-key "r" 'ivy-resume)

  (define-key ivy-minibuffer-map (kbd "S-SPC") nil)
  (define-key ivy-minibuffer-map [tab] 'ivy-partial)

  (evil-declare-not-repeat 'swiper)

  (ivy-add-actions 'counsel-find-file
                   '(("F" (lambda (x)
                            (with-ivy-window (insert (file-relative-name x))))
                      "insert relative file name")

                     ("B" (lambda (x)
                            (with-ivy-window
                              (insert
                               (file-name-nondirectory
                                (replace-regexp-in-string "/\\'" "" x)))))
                      "insert file name without any directory information"))))

(use-package ivy-hydra)

(use-package ivy-rich
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer)

  (setq ivy-virtual-abbreviate 'full)
  (setq ivy-rich-switch-buffer-align-virtual-buffer t)
  (setq ivy-rich-abbreviate-paths t)
  (setq ivy-rich-switch-buffer-name-max-length 64))

(use-package json-mode)

(use-package magit
  :config
  ;; Needed for success status message to be shown.
  (setq magit-auto-revert-mode nil)

  (setq magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-v1))

(use-package monky)

(use-package outline
  :init
  (if (version< emacs-version "25.1")
      (add-hook 'ediff-prepare-buffer-hook #'show-all)
    (add-hook 'ediff-prepare-buffer-hook #'outline-show-all)))

(use-package package-utils)

(use-package rainbow-mode :diminish rainbow-mode)

(use-package rainbow-delimiters :diminish rainbow-delimiters-mode)

(use-package recentf
  :config
  (recentf-mode)
  (setq recentf-max-menu-items 25))

(use-package smartparens
  :diminish smartparens-mode

  :config
  (sp-use-paredit-bindings)
  ;; (sp-pair "\"" nil :actions :rem)
  (jco/define-bindings smartparens-mode-map
                       '(("M-?" . sp-convolute-sexp)
                         ("C-k" . sp-kill-hybrid-sexp)
                         ("M-j" . sp-join-sexp)))
  )

(use-package speed-type)

(use-package string-inflection
  :config
  (evil-leader/set-key "s i" 'string-inflection-all-cycle))

(use-package sx
  :config
  (with-eval-after-load "sx-question-mode"
    (bind-keys :map sx-question-mode-map
               ("j" . scroll-up-line)
               ("k" . scroll-down-line))))

(use-package unkillable-scratch
  :init
  (unkillable-scratch))

(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode))

(use-package yaml-mode
  :init

  (add-hook 'yaml-mode-hook
            (lambda ()
              (setq evil-shift-width 2))))

(use-package zeal-at-point
  :config
  (evil-leader/set-key "z" 'zeal-at-point)
  (setq zeal-at-point-docsets '(c cpp))
  (add-to-list 'zeal-at-point-mode-alist '(c++-mode . ("cpp" "qt"))))

(require 'server)
(when (not (server-running-p))
  (server-start))

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

(global-set-key (kbd "C-x a r") 'align-regexp)
(defadvice align-regexp (around align-regexp-with-spaces activate compile)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))

(put 'narrow-to-region 'disabled nil)

(add-hook 'help-mode-hook
          (lambda ()
            ;; do not treat "-" as a word separator
            (modify-syntax-entry ?- "w")))

(use-package info+
  :config
  (bind-keys :map Info-mode-map
             ("<tab>"     . Info-next-reference)
             ("<backtab>" . Info-prev-reference)))

(require 'help-mode)
(bind-keys :map help-mode-map
           ("<tab>"     . forward-button)
           ("<backtab>" . backward-button))

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

(setq large-file-warning-threshold nil)
(setq safe-local-variable-values
      '((org-archive-location . "::* Archived Tasks")))

(put 'erase-buffer 'disabled nil)

(windmove-default-keybindings)

(require 'ibuffer)

(dolist (map (list dashboard-mode-map ibuffer-mode-map package-menu-mode-map))
  (define-key map "\C-w" 'evil-window-map))

(global-set-key (kbd "C-x b") 'ibuffer)

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (turn-off-fci-mode)))

(setq compilation-scroll-output t)

(require 'qmake-mode)

(use-package iedit)

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t))

(eval-after-load "info" '(require 'info+))

(diminish 'abbrev-mode)

(provide 'init-common)

;;; init-common.el ends here
