(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

(defun set-yasnippet-fixed-indent ()
  (setq-local yas-indent-line 'fixed))

(add-hook 'haskell-mode-hook
          (lambda ()
            (let* ((paths (split-string (shell-command-to-string
                                         "stack path --bin-path 2> /dev/null")
                                        path-separator))
                   (my-bin-path (car paths)))
              (setenv "PATH" (concat (getenv "PATH") ":" my-bin-path))
              (add-to-list 'exec-path my-bin-path))

            (ghc-init)
            (subword-mode -1)

            (setq haskell-interactive-popup-errors nil)
            (setq haskell-process-auto-import-loaded-modules t)
            (setq haskell-process-log t)
            (setq haskell-process-suggest-remove-import-lines t)
            (setq haskell-process-type 'auto)
            (setq haskell-tags-on-save t)

            (haskell-doc-mode)

            (hindent-mode)
            ;; (haskell-indentation-mode)

            ;; (global-unset-key [tab])
            ;; (local-set-key [tab] (lambda ()
            ;;                        (interactive)
            ;;                        (tab-indent-or-complete 1)))
            (setq tab-stop-list
                  (loop for i from 0 upto 120 by 2 collect i))
            (setq evil-shift-width 2)
            (set-yasnippet-fixed-indent)

            (company-mode)
            ;; (define-key haskell-indentation-mode-map (kbd "RET") nil)
            ;; (define-key global-map (kbd "RET") 'newline-and-indent-relative)
            (add-to-list 'company-backends 'company-cabal)
            (add-to-list 'company-backends 'company-ghc)
            (add-to-list 'company-backends 'company-dabbrev-code)
            (setq company-ghc-show-info t)

            ;; (define-key yas-minor-mode-map (kbd "TAB") nil)
            ;; (define-key evil-motion-state-map (kbd "RET") nil)

            (setq ebal-operation-mode 'stack)
            (evil-leader/set-key "e i" 'ebal-init)
            (evil-leader/set-key "e e" 'ebal-execute)))

(eval-after-load 'haskell-mode
  '(jco/define-bindings haskell-mode-map
                        '(("<f8>" . haskell-navigate-imports)
                          ("S-<f8>" . haskell-sort-imports)
                          ("C-c C-a" . haskell-align-imports)
                          ("C-c C-l" . haskell-process-load-file)
                          ("C-c C-z" . haskell-interactive-switch)
                          ("C-c C-n C-t" . haskell-process-do-type)
                          ("C-c C-n C-i" . haskell-process-do-info)
                          ("C-c C-n C-c" . haskell-process-cabal-build)
                          ("C-c C-n c" . haskell-process-cabal)
                          ("C-c C-o" . haskell-compile)
                          ("C-c C-k" . haskell-interactive-mode-clear))))


(eval-after-load 'haskell-cabal
  '(jco/define-bindings haskell-cabal-mode-map
                        '(("C-c C-z" . haskell-interactive-switch)
                          ("C-c C-k" . haskell-interactive-mode-clear)
                          ("C-c C-c" . haskell-process-cabal-build)
                          ("C-c c" . haskell-process-cabal)
                          ("C-c C-o" . haskell-compile))))

(provide 'init-haskell)
