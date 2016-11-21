(defun set-yasnippet-fixed-indent ()
  (setq-local yas-indent-line 'fixed))

(add-hook 'haskell-mode-hook
          (lambda ()
            (let* ((paths (split-string (shell-command-to-string
                                         "stack path --local-install-root 2> /dev/null")
                                        path-separator))
                   (bin-path (concat (string-trim-right (car paths)) "/bin")))
              (setenv "PATH" (concat bin-path ":" (getenv "PATH")))
              (add-to-list 'exec-path bin-path))

            (subword-mode -1)

            ;; (setq flycheck-check-syntax-automatically '(save new-line))
            (flycheck-mode -1)
            (diminish 'flycheck-mode)

            (when (and (not (eq system-type 'windows-nt))
                       (not (s-contains? "org-src" (buffer-name)))
                       (not (s-contains? "*temp" (buffer-name))))
              (intero-mode)
              (diminish 'intero-mode))

            (setq haskell-interactive-popup-errors nil)
            (setq haskell-process-auto-import-loaded-modules t)
            (setq haskell-process-log t)
            (setq haskell-process-suggest-remove-import-lines t)
            (setq haskell-process-type 'auto)
            ;; (setq haskell-tags-on-save t) ;; doesn't work for Fish

            (haskell-doc-mode)
            (diminish 'haskell-doc-mode)

            (hindent-mode)
            (diminish 'hindent-mode)

            (setq tab-stop-list
                  (loop for i from 0 upto 120 by 2 collect i))
            (setq evil-shift-width 2)
            (set-yasnippet-fixed-indent)

            (setq ebal-operation-mode 'stack)
            (evil-leader/set-key "e i" 'ebal-init)
            (evil-leader/set-key "e e" 'ebal-execute)
            (evil-leader/set-key "q a" 'jco/align-last-eq)

            (jco/define-bindings
             haskell-mode-map
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
               ("C-c C-k" . haskell-interactive-mode-clear)))

            (jco/define-bindings
             haskell-cabal-mode-map
             '(("C-c C-z" . haskell-interactive-switch)
               ("C-c C-k" . haskell-interactive-mode-clear)
               ("C-c C-c" . haskell-process-cabal-build)
               ("C-c c" . haskell-process-cabal)
               ("C-c C-o" . haskell-compile)))))

(provide 'init-haskell)
