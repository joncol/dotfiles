(require 'cl)

;;; Avoid the empty (custom-set-faces) at end of init.el.
(setq custom-file (expand-file-name (concat user-emacs-directory "custom.el")))

(setq ad-redefinition-action 'accept)

(global-set-key (kbd "M-w") 'ace-window)

(tool-bar-mode -1)
(global-auto-revert-mode)
(global-font-lock-mode)
(electric-pair-mode)
(show-paren-mode)
(rainbow-mode)
(global-hl-line-mode)
;; (global-linum-mode)

(modify-syntax-entry ?_ "w") ;; do not treat "_" as a word separator

(use-package server
  :demand
  :config
  (when (not (server-running-p))
    (server-start)))

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
(setq display-time-string-forms '(24-hours ":" minutes))
(display-time-mode)

(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-x a r") 'align-regexp)
(defadvice align-regexp (around align-regexp-with-spaces activate compile)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))

(jco/define-bindings Info-mode-map
                     '(("<tab>" . Info-next-reference)
                       ("<backtab>" . Info-prev-reference)))

(require 'help-mode)
(jco/define-bindings help-mode-map
                     '(("<tab>" . forward-button)
                       ("<backtab>" . backward-button)))

(global-set-key (kbd "C-c C-b") 'help-go-back)
(global-set-key (kbd "C-c C-f") 'help-go-forward)

(when (memq window-system '(mac ns))
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(let ((my-bin-path (expand-file-name "~/.local/bin")))
  (setenv "PATH" (concat (getenv "PATH") ":" my-bin-path))
  (add-to-list 'exec-path my-bin-path t))

(setq cider-show-error-buffer 'nil)
(setq ecb-tip-of-the-day nil)

(setq large-file-warning-threshold nil)
(setq safe-local-variable-values
      '((org-archive-location . "::* Archived Tasks")))

(use-package recentf
  :config
  (recentf-mode)
  (setq recentf-max-menu-items 25))

(setq fortune-dir "/usr/share/games/fortunes")
(setq fortune-file "/usr/share/games/fortunes")

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

(use-package fancy-narrow
  :diminish fancy-narrow-mode
  :config
  (put 'narrow-to-region 'disabled nil))

(ace-link-setup-default (kbd "f"))

(use-package fuzzy
  :config
  (turn-on-fuzzy-isearch))

(require 'qmake-mode)
(require 'iedit)

(require 'diminish)
(eval-after-load "company" '(diminish 'company-mode))

(global-undo-tree-mode)
(diminish 'undo-tree-mode)
(setq undo-tree-visualizer-diff t)
(setq undo-tree-visualizer-timestamps t)

(global-set-key (kbd "C-x o") 'ace-window)

(eval-after-load "info" '(require 'info+))

(setq paradox-github-token "68afbf92086e59ec7e2ed974a8bad7ecf7b39127")

(diminish 'abbrev-mode)

(use-package ace-isearch
  :diminish ace-isearch-mode
  :config
  (global-ace-isearch-mode))

(use-package ace-jump-helm-line-mode
  :diminish ace-jump-helm-line-mode
  :bind (:map helm-map
         ("C-'" . ace-jump-helm-line)))

(provide 'init-common)
