;;; #init-common.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

;;; Avoid the empty (custom-set-faces) at end of init.el.
(setq custom-file (expand-file-name (concat user-emacs-directory "custom.el")))
(load custom-file)

(setq ad-redefinition-action 'accept)

(setq-default explicit-shell-file-name "/bin/bash")

(define-minor-mode jco/my-keys-mode
  "Minor mode for my personal keybindings."
  :global t
  :keymap (make-sparse-keymap))

(add-hook 'jco/my-keys-mode-hook
          (lambda ()
            (evil-normal-state)))

(jco/my-keys-mode)

;; (when (version<= "26" emacs-version)
;;   (global-display-line-numbers-mode))
(column-number-mode)
(menu-bar-mode -1)
(when (display-graphic-p)
  (scroll-bar-mode -1))
(tool-bar-mode -1)
(global-auto-revert-mode)
(add-to-list 'revert-without-query ".*\\.pdf\\'")
(global-font-lock-mode)
(setq select-enable-primary t)
(global-whitespace-mode)
(setq calendar-week-start-day 1)

(defun prevent-whitespace-mode-for-magit ()
  (not (derived-mode-p 'magit-mode)))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(winner-mode)
(define-key jco/my-keys-mode-map (kbd "C-x C-j") (lambda ()
                                                   (interactive)
                                                   (dired ".")))

(define-key jco/my-keys-mode-map (kbd "C-c j")
  (lambda ()
    (interactive)
    (require 'calendar)
    (let* ((year (caddr (calendar-current-date)))
           (file-name (format "~/ledgers/%s.journal" year)))
      (find-file (expand-file-name file-name)))))

(evil-leader/set-key "x b" 'browse-url)
(evil-leader/set-key "x w" 'woman)

(when (eq system-type 'gnu/linux)
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "google-chrome-stable"))

(modify-syntax-entry ?_ "w") ;; do not treat "_" as a word separator

(defalias 'yes-or-no-p 'y-or-n-p)
(setq auto-save-default nil)
(setq make-backup-files nil)

(setq evil-motion-state-modes
      (append '(debugger-mode) evil-motion-state-modes))

(add-hook 'doc-view-mode-hook
          (lambda ()
            (when (fboundp 'nlinum-mode)
              (nlinum-mode -1))
            (define-key doc-view-mode-map "\C-w" 'evil-window-map)))

(add-hook 'messages-buffer-mode-hook 'ansi-color-for-comint-mode-on)

(add-hook 'conf-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")))

(add-hook 'scss-mode-hook
          (lambda ()
            (modify-syntax-entry ?- "w")))

(add-hook 'sql-mode-hook
          (lambda ()
            (modify-syntax-entry ?- "w" sql-mode-syntax-table)))

(add-hook 'TeX-mode-hook
          (lambda ()
            (setq evil-shift-width 2)))

(defadvice view-emacs-news (after evil-motion-state-in-news-view
                                  activate compile)
  "Enable evil motion state."
  (evil-motion-state))

(defadvice view-emacs-problems (after evil-motion-state-in-problems-view
                                      activate compile)
  "Enable evil motion state."
  (evil-motion-state))

(jco/define-bindings minibuffer-inactive-mode-map
                     '(("C-n" . ido-next-match)
                       ("C-p" . ido-prev-match)))

(setq compilation-scroll-output t)

(use-package ace-isearch
  :disabled t
  :config
  (global-ace-isearch-mode))

(use-package ace-link
  :init
  (ace-link-setup-default "f"))

(use-package ace-window
  :init
  (global-set-key [remap other-window] 'ace-window))

(use-package aggressive-indent
  :config
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c-mode 'c++-mode 'java-mode)
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line))))))

(use-package ahk-mode)

(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-ivy
  :config
  (all-the-icons-ivy-setup))

(use-package anzu
  :config
  (global-anzu-mode))

(use-package auto-yasnippet
  :config
  (evil-leader/set-key "y c" #'aya-create)
  (evil-leader/set-key "y e" #'aya-expand))

(use-package avy
  :config
  (setq avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o)) ;; Colemak specific

  (evil-leader/set-key "f" 'evil-avy-goto-char)
  (evil-leader/set-key "#" 'evil-avy-goto-line)
  (evil-leader/set-key "F" 'evil-avy-goto-word-or-subword-1)
  (evil-leader/set-key "/" 'avy-goto-char-timer)
  (evil-declare-not-repeat 'avy-goto-char-timer)
  (avy-setup-default)
  (setq avy-case-fold-search nil))

(use-package bookmark+
  :straight t
  :ensure nil
  :defer t
  :config
  (bmkp-info-auto-bookmark-mode))

(use-package buffer-move
  :if (not (eq system-type 'windows-nt))
  :config
  (jco/define-bindings jco/my-keys-mode-map
                       '(("<C-S-up>"    . buf-move-up)
                         ("<C-S-down>"  . buf-move-down)
                         ("<C-S-left>"  . buf-move-left)
                         ("<C-S-right>" . buf-move-right))))

(use-package calfw
  :commands cfw:open-org-calendar
  :config
  (setq cfw:display-calendar-holidays nil))

(use-package calfw-org
  :after calfw)

(use-package counsel
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-r" . counsel-recentf)
         ("C-c p s a" . counsel-projectile-ag))
  :config
  (jco/define-bindings jco/my-keys-mode-map
                       '(("C-h f" . counsel-describe-function)
                         ("C-h v" . counsel-describe-variable)
                         ("C-h S" . counsel-info-lookup-symbol)))
  (evil-leader/set-key "x z" 'counsel-fzf)
  (when (eq system-type 'windows-nt)
    (setq-default counsel-ag-base-command
                  "ag --vimgrep --nocolor --nogroup %s")))

(use-package counsel-projectile
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))
  :config
  (counsel-projectile-mode)
  (setq counsel-projectile-ag-initial-input '(thing-at-point 'symbol t))
  (setq counsel-projectile-rg-initial-input '(thing-at-point 'symbol t)))

(use-package csv-mode)

(use-package cypher-mode)

(use-package deadgrep
  :bind (:map deadgrep-mode-map
         ("j" . deadgrep-forward)
         ("k" . deadgrep-backward))
  :init
  (evil-leader/set-key "d g" 'deadgrep)
  :config
  (evil-set-initial-state 'deadgrep-mode 'emacs))

(use-package desktop
  :config
  (push ".*" desktop-clear-preserve-buffers))

(use-package dired+
  :straight t
  :ensure nil
  :after dired
  :config
  (diredp-toggle-find-file-reuse-dir 1))

(use-package direnv
  :config
  (direnv-mode))

(defun mydired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first, before adding marks."
  (mydired-sort))

(use-package dired-narrow
  :after dired+
  :bind (:map dired-mode-map
         ("/" . dired-narrow)))

(use-package dired-subtree
  :after dired+
  :config
  (bind-keys :map dired-mode-map
    ("i" . dired-subtree-insert)
    (";" . dired-subtree-remove)))

(use-package docker-compose-mode
  :defer t)

(use-package dockerfile-mode
  :defer t)

(use-package ecb
  :defer t
  :config
  (setq ecb-tip-of-the-day nil))

(use-package eclim
  :config
  ;; (setq eclimd-autostart t)
  (setq eclimd-autostart-with-default-workspace t)
  (setq eclim-eclipse-dirs "~/eclipse/java-oxygen/eclipse")
  (setq eclim-executable
        (expand-file-name "~/.p2/pool/plugins/org.eclim_2.7.2/bin/eclim"))
  (setq eclimd-default-workspace (expand-file-name "~/eclipse-workspace"))
  (evil-set-initial-state 'eclim-problems-mode 'emacs)
  (evil-set-initial-state 'eclim-project-mode 'emacs)
  (add-hook 'java-mode-hook
            (lambda ()
              (eclim-mode)
              (evil-leader/set-key "e b" 'eclim-project-build)
              (evil-leader/set-key "e c" 'eclim-project-create)
              (evil-leader/set-key "e r" 'eclim-run-class)
              (setq help-at-pt-display-when-idle t)
              (setq help-at-pt-timer-delay 0.1)
              (help-at-pt-set-timer)
              (jco/define-bindings
               java-mode-map
               '(("M-g M-n" . eclim-problems-next-same-file)
                 ("M-g n" . eclim-problems-next-same-file)
                 ("M-g M-p" . eclim-problems-prev-same-file)
                 ("M-g p" . eclim-problems-prev-same-file))))))

(defmacro jco/set-eyebrowse-win-bindings ()
  "Generate evil-leader bindings for switching eyebrowse windows."
  `(progn ,@(mapcar
             (lambda (i)
               (let ((sym (intern
                           (format "eyebrowse-switch-to-window-config-%d" i))))
                 (evil-leader/set-key (format "w %d" i) sym)))
             (number-sequence 0 9))))

(use-package eyebrowse
  :init
  (setq eyebrowse-keymap-prefix "")
  :config
  (eyebrowse-mode)
  (setq eyebrowse-mode-line-separator " ")
  (setq eyebrowse-new-workspace t)
  (jco/set-eyebrowse-win-bindings)
  (evil-leader/set-key "w c" 'eyebrowse-close-window-config)
  (set-face-foreground 'mode-line-emphasis "Red"))

(use-package ggtags
  :if (eq system-type 'windows-nt))

(use-package ghub)

(use-package gist
  :config
  (setq gist-view-gist t))

(use-package gradle-mode
  :config
  (add-hook 'java-mode-hook
            (lambda ()
              (setq gradle-executable-path "/opt/gradle-4.6/bin/gradle")
              (gradle-mode)
              (evil-leader/set-key "g r"
                (lambda ()
                  (interactive)
                  (gradle-run "run")))
              (evil-leader/set-key "t t"
                (lambda ()
                  (interactive)
                  (gradle-run "test --info")))
              (evil-leader/set-key "t s" 'gradle-single-test))))

(use-package elec-pair
  :init
  (electric-pair-mode)
  :config
  ;; (setq electric-pair-preserve-balance nil)
  (setq electric-pair-skip-whitespace nil)
  (setq electric-pair-delete-adjacent-pairs nil))

(use-package esup
  :defer t
  :config
  (define-key esup-mode-map "\C-w" 'evil-window-map))

(use-package evil-ediff)

(use-package evil-god-state
  :config
  (evil-define-key 'normal jco/my-keys-mode-map (kbd "SPC")
    'evil-execute-in-god-state)
  (evil-define-key 'god jco/my-keys-mode-map [escape] 'evil-god-state-bail))

(use-package evil-ledger
  :after ledger-mode
  :config
  (setq evil-ledger-sort-key "S")
  (add-hook 'ledger-mode-hook #'evil-ledger-mode))

(use-package evil-magit
  :after magit
  :config
  (setq magit-rebase-arguments '("--autosquash" "--autostash"))
  (setq evil-motion-state-modes
        (append '(magit-submodule-list-mode) evil-motion-state-modes))
  (add-hook 'magit-mode-hook
            (lambda ()
              (evil-local-set-key 'normal (kbd "SPC")
                                  'magit-diff-show-or-scroll-up)
              (when (version<= "26" emacs-version)
                (display-line-numbers-mode -1)))))

(use-package evil-numbers
  :bind (("C-c +" . evil-numbers/inc-at-pt)
         ("C-c -" . evil-numbers/dec-at-pt)))

(use-package expand-region
  :config
  (define-key jco/my-keys-mode-map (kbd "C-=") 'er/expand-region))

(use-package f)

(use-package fireplace)

(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq flycheck-pos-tip-timeout 0))

(use-package flycheck-pos-tip
  :init
  (flycheck-pos-tip-mode))

(use-package flycheck-rtags
  :defer t)

(use-package fortune
  :if (not (eq system-type 'windows-nt))
  :disabled t
  :config
  (setq fortune-dir "/usr/share/games/fortunes")
  (setq fortune-file "/usr/share/games/fortunes"))

(use-package fortune-cookie
  :if (not (eq system-type 'windows-nt))
  :disabled t
  :config
  (setq fortune-cookie-cowsay-enable t)
  (setq fortune-cookie-cowsay-args "-f tux")
  (fortune-cookie-mode))

(use-package fuzzy
  :config
  (turn-on-fuzzy-isearch))

(use-package nlinum
  :disabled (version< "26" emacs-version)
  :init
  (global-nlinum-mode))

(use-package git-gutter+
  :disabled t
  :if (not (eq system-type 'windows-nt))
  :config
  (global-git-gutter+-mode))

(use-package git-gutter-fringe+
  :disabled t
  :after nlinum
  :if (not (eq system-type 'windows-nt)))

(use-package git-link
  :after magit
  :config
  (setq git-link-open-in-browser t)
  (evil-leader/set-key "g h" 'git-link-homepage)
  (evil-leader/set-key "g l" 'git-link))

(use-package glsl-mode)

(use-package google-this
  :defer t
  :init
  (evil-leader/set-key "x g" 'google-this))

(use-package guide-key
  :config
  (guide-key-mode)
  ;; (setq guide-key/popup-window-position "right")
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c C-r")))

(use-package help-fns+
  :disabled t
  :defer 1)

;; To get colors in html export of org-mode code snippets.
(use-package htmlize)

(use-package imenu-anywhere
  :config
  (evil-leader/set-key "x m" #'imenu-anywhere))

(use-package ivy
  :bind (("C-s" . swiper)
         ("C-x C-b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers nil)
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-selectable-prompt t)
  (evil-leader/set-key "b" 'ivy-switch-buffer)
  (evil-leader/set-key "r" 'ivy-resume)
  (define-key ivy-minibuffer-map (kbd "S-SPC") nil)
  (define-key ivy-minibuffer-map [tab] 'ivy-partial)
  (setq ivy-on-del-error-function nil)
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

(use-package ivy-rich
  :config
  ;; (ivy-set-display-transformer 'ivy-switch-buffer
  ;;                              'ivy-rich-switch-buffer-transformer)
  (setq ivy-virtual-abbreviate 'full)
  (setq ivy-rich-switch-buffer-align-virtual-buffer t)
  (setq ivy-rich-abbreviate-paths t)
  (setq ivy-rich-switch-buffer-name-max-length 64))

(use-package ivy-rtags
  :after rtags
  :config
  (setq rtags-display-result-backend 'ivy))

(use-package json-mode
  :defer t)

(use-package kurecolor
  :defer t)

(use-package ledger-mode
  :defer t
  :mode "\\.journal\\'"
  :config
  (setq ledger-mode-should-check-version nil)
  (setq ledger-report-links-in-register nil)
  (setq ledger-binary-path "hledger")
  (add-to-list 'ledger-reports
               `("monthly expenses"
                 ,(concat "%(binary) -f %(ledger-file) balance expenses"
                          " --tree --no-total --row-total --average --monthly"))
               t)
  (add-hook 'ledger-mode-hook
            (lambda ()
              (turn-off-fci-mode))))

(use-package lorem-ipsum
  :defer t)

(defun jco/magit-kill-buffers ()
  "Restore window configuration and kill all Magit buffers."
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (mapc #'kill-buffer buffers)))

(use-package magit
  :defer 1
  :config
  ;; Fix regression where error message is shown when using magit-status while
  ;; having global-whitespace-mode enabled.
  (add-function :before-while whitespace-enable-predicate
    'prevent-whitespace-mode-for-magit)

  ;; Needed for success status message to be shown.
  (setq magit-auto-revert-mode nil)

  (global-magit-file-mode)
  (setq magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-v1)
  (evil-leader/set-key "v l" 'magit-log-buffer-file)
  (evil-leader/set-key "v b" 'magit-blame)
  (setq magit-blame-disabled-modes '(fci-mode))
  (bind-key "q" #'jco/magit-kill-buffers magit-status-mode-map)
  (add-hook 'git-commit-setup-hook
            (lambda ()
              (setq fill-column 72)
              (fci-mode)
              (modify-syntax-entry ?- "w")
              (git-commit-turn-on-flyspell))))

(use-package magit-org-todos
  :disabled t
  :after magit
  :config
  (magit-org-todos-autoinsert))

(use-package minions
  :config
  (minions-mode)
  (setq minions-mode-line-lighter "#"))

(use-package monky
  :defer t)

(use-package mustache-mode
  :mode "\\.mustache\\'"
  :defer t)

(use-package nginx-mode)

(use-package org-gcal
  :after calfw
  :config
  (require 'my-secrets "~/.emacs.d/lisp/my-secrets.el.gpg")
  (setq org-gcal-file-alist
        '(("jonas.collberg@zimpler.com" . "~/Sync/emacs/gcal_zimpler.org"))))

(use-package outline
  :init
  (if (version< emacs-version "25.1")
      (add-hook 'ediff-prepare-buffer-hook #'show-all)
    (add-hook 'ediff-prepare-buffer-hook #'outline-show-all)))

(use-package package-utils)

(use-package pdf-tools
  :if (and (not (eq system-type 'windows-nt))
           (display-graphic-p))
  :defer t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(use-package rainbow-mode)

(use-package rainbow-delimiters)

(use-package recentf
  :defer t
  :config
  (recentf-mode)
  (setq recentf-max-menu-items 25))

(defun sp--org-skip-markup (ms mb me)
  (save-excursion
    (and (progn
           (goto-char mb)
           (save-match-data (looking-back "\\sw\\|\\s_\\|\\s.")))
         (progn
           (goto-char me)
           (save-match-data (looking-at "\\sw\\|\\s_\\|\\s."))))))

(use-package smartparens
  :defer t
  :config
  (sp-use-paredit-bindings)
  ;; (sp-pair "\"" nil :actions :rem)
  (show-smartparens-global-mode)
  (setq sp-navigate-interactive-always-progress-point t)
  (jco/define-bindings global-map
                       '(("M-(" . (lambda (&optional arg)
                                    (interactive "P")
                                    (sp-wrap-with-pair "(")))
                         ("M-[" . (lambda (&optional arg)
                                    (interactive "P")
                                    (sp-wrap-with-pair "[")))
                         ("M-{" . (lambda (&optional arg)
                                    (interactive "P")
                                    (sp-wrap-with-pair "{")))
                         ("M-\"" . (lambda (&optional arg)
                                     (interactive "P")
                                     (sp-wrap-with-pair "\"")))))
  (jco/define-bindings smartparens-mode-map
                       '(("M-?" . sp-convolute-sexp)
                         ("C-k" . sp-kill-hybrid-sexp)
                         ("M-j" . sp-join-sexp)
                         ("M-C" . sp-clone-sexp)
                         ("C-M-n" . sp-next-sexp)
                         ("C-M-p" . sp-previous-sexp)
                         ("C-M-e" . sp-up-sexp)
                         ("C-M-d" . sp-down-sexp)
                         ("C-M-a" . sp-backward-down-sexp)
                         ("C-S-d" . sp-beginning-of-sexp)
                         ("C-S-a" . sp-end-of-sexp)))
  (sp-with-modes (set-difference sp--lisp-modes sp-clojure-modes)
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p))
    (sp-local-pair "`" nil
                   :skip-match
                   (lambda (ms mb me)
                     (cond
                      ((equal ms "'")
                       (or (sp--org-skip-markup ms mb me)
                           (not (sp-point-in-string-or-comment))))
                      (t (not (sp-point-in-string-or-comment)))))))
  (sp-with-modes sp-clojure-modes
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" nil :actions nil)))

(use-package speed-type
  :defer t)

(use-package string-inflection
  :config
  (evil-leader/set-key "s i" 'string-inflection-all-cycle)
  (evil-leader/set-key "s s" 'string-inflection-underscore)
  (evil-leader/set-key "s k" 'string-inflection-kebab-case)
  (evil-leader/set-key "s c" 'string-inflection-camelcase))

(defun jco/camel-case-to-sentence (text)
  "Convert TEXT from camelCase to a sentence.
Example: `helloWorld` becomes `Hello world`."
  (interactive)
  (let* ((snake (string-inflection-underscore-function text))
         (words (replace-regexp-in-string "_" " " snake)))
    (jco/capitalize-first-char words)))

(defun jco/cpp-class-name ()
  "Return the class name corresponding to the name of the current buffer."
  (interactive)
  (let* ((base-name (file-name-base buffer-file-name)))
    (string-inflection-camelcase-function base-name)))

(use-package super-save
  :config
  (super-save-mode))

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
  :config
  (volatile-highlights-mode))

(use-package yaml-mode
  :mode "\\.yml\\'"
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (modify-syntax-entry ?- "w")
              (setq evil-shift-width 2))))

(use-package zeal-at-point
  :config
  (evil-leader/set-key "z" 'zeal-at-point)
  (setq zeal-at-point-docsets '(c cpp))
  (add-to-list 'zeal-at-point-mode-alist '(c++-mode . ("cpp" "qt"))))

(require 'server)

(use-package server
  :if (and (not (server-running-p))
           (not (daemonp)))
  :defer 1
  :config
  (server-start))

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

(define-key jco/my-keys-mode-map (kbd "C-x a r") 'align-regexp)
(defadvice align-regexp (around align-regexp-with-spaces activate compile)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))

(put 'narrow-to-region 'disabled nil)

(setq help-window-select t)

(add-hook 'help-mode-hook
          (lambda ()
            ;; do not treat "-" as a word separator
            (modify-syntax-entry ?- "w")))

(add-hook 'makefile-gmake-mode-hook
          (lambda ()
            ;; do not treat "-" as a word separator
            (modify-syntax-entry ?- "w")))

(add-hook 'sql-mode-hook
          (lambda ()
            ;; do not treat "-" as a word separator
            (modify-syntax-entry ?- "w")))

(jco/define-bindings Info-mode-map
                     '(("<tab>"     . Info-next-reference)
                       ("<backtab>" . Info-prev-reference)))

(require 'help-mode)
(bind-keys :map help-mode-map
  ("<tab>"     . forward-button)
  ("<backtab>" . backward-button))

(jco/define-bindings jco/my-keys-mode-map '(("C-c C-b" . help-go-back)
                                            ("C-c C-f" . help-go-forward)))

(let ((my-bin-path (expand-file-name "~/.local/bin"))
      (fzf-bin-path (expand-file-name "~/.fzf/bin")))
  (setenv "PATH" (concat (getenv "PATH") ":" my-bin-path))
  (add-to-list 'exec-path my-bin-path t)
  (add-to-list 'exec-path fzf-bin-path t))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :defer t
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (exec-path-from-shell-initialize))))

(setq large-file-warning-threshold nil)
(setq safe-local-variable-values
      '((org-archive-location . "::* Archived Tasks")))

(put 'erase-buffer 'disabled nil)

(windmove-default-keybindings)

(require 'ibuffer)

(dolist (map (list ibuffer-mode-map package-menu-mode-map))
  (define-key map "\C-w" 'evil-window-map))

(define-key jco/my-keys-mode-map (kbd "C-x b") 'ibuffer)

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (turn-off-fci-mode)))

(use-package iedit)

(use-package info+
  :straight t
  :ensure nil
  :after info)

(use-package s
  :config
  (setq user-mail-address
        (concat (s-replace " " "." (downcase user-full-name)) "@"
                (if (jco/at-office-p)
                    "zimpler.com"
                  "gmail.com"))))

(use-package try)

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t))

(use-package visual-fill-column)

(use-package wgrep)

(use-package which-key
  :config
  (which-key-mode))

(evil-leader/set-key "x o" 'occur)

(when (display-graphic-p)
  (global-unset-key (kbd "C-x C-z")))

(setq vc-follow-symlinks nil)

(provide 'init-common)

;;; init-common.el ends here
