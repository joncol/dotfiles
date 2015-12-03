;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-m") 'helm-semantic-or-imenu)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

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
      ;; helm-gtags-prefix-key "\C-cg"
      helm-gtags-suggested-key-mapping t
      helm-ag-base-command "ag --nocolor --nogroup --line-numbers --smart-case
 --ignore #*#;TAGS;*.html;*.json;*.map;*.opensdf;*.pdf;*.sdf"
      helm-ag-insert-at-point 'word)

(when (not (eq system-type 'windows-nt))
  (setq helm-ag-ignore-patterns
        '("#*#" "TAGS" "*.html" "*.json" "*.log" "*.map" "*.opensdf" "*.pdf"
          "*.sdf" "backups" "coverage" "doc" "elpa" "semanticdb")))

;; enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(helm-mode)

;;; projectile
(projectile-global-mode)
(when (not (eq system-type 'windows-nt))
  (setq projectile-indexing-method 'native))
(setq projectile-enable-caching t)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(provide 'init-helm-and-projectile)
