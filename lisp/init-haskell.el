(add-to-list 'load-path "~/repos/ghc-mod/elisp")
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

(setq haskell-interactive-popup-errors nil)

(setq haskell-process-auto-import-loaded-modules t)
(setq haskell-process-log t)
(setq haskell-process-suggest-remove-import-lines t)
(setq haskell-process-type 'cabal-repl)

(setq haskell-tags-on-save t)

(defun newline-and-indent-relative ()
  (interactive)
  (newline)
  (indent-to-column (save-excursion
                      (forward-line -1)
                      (back-to-indentation)
                      (current-column))))

(add-hook 'haskell-mode-hook
          (lambda ()
            (ghc-init)

            (turn-on-haskell-doc-mode)
            (remove-hook 'haskell-mode-hook 'turn-on-haskell-indent)

            (turn-on-haskell-indentation)
            (global-unset-key [tab])
            (local-set-key [tab] (lambda ()
                                   (interactive)
                                   (tab-indent-or-complete 1)))
            (setq tab-stop-list
                  (loop for i from 0 upto 120 by 2 collect i))
            (setq evil-shift-width 2)
            (company-mode)
            (define-key haskell-indentation-mode-map (kbd "RET") nil)
            (define-key global-map (kbd "RET") 'newline-and-indent-relative)
            (add-to-list 'company-backends 'company-cabal)
            (add-to-list 'company-backends 'company-ghc)
            (setq company-ghc-show-info t)

            (define-key yas-minor-mode-map (kbd "TAB") nil)
            (define-key evil-motion-state-map (kbd "RET") nil)
            (define-key evil-normal-state-map (kbd "M-.") nil)

            (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
              (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
              (add-to-list 'exec-path my-cabal-path))))

(eval-after-load 'haskell-mode
  '(jco/define-bindings haskell-mode-map
                        '(("<f8>" . haskell-navigate-imports)
                          ("S-<f8>" . haskell-sort-imports)
                          ("C-c C-l" . haskell-process-load-or-reload)
                          ("C-c C-z" . haskell-interactive-switch)
                          ("C-c C-n C-t" . haskell-process-do-type)
                          ("C-c C-n C-i" . haskell-process-do-info)
                          ("C-c C-n C-c" . haskell-process-cabal-build)
                          ("C-c C-n c" . haskell-process-cabal)
                          ("C-c C-o" . haskell-compile)
                          ("SPC" . haskell-mode-contextual-space))))


(eval-after-load 'haskell-cabal
  '(jco/define-bindings haskell-cabal-mode-map
                        '(("C-c C-z" . haskell-interactive-switch)
                          ("C-c C-k" . haskell-interactive-mode-clear)
                          ("C-c C-c" . haskell-process-cabal-build)
                          ("C-c c" . haskell-process-cabal)
                          ("C-c C-o" . haskell-compile))))

(provide 'init-haskell)
