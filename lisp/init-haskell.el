;;; #init-haskell.el --- Haskell config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package intero
  :defer t
  :config
  (evil-leader/set-key "x r" 'intero-restart))

(use-package haskell-mode
  :defer t
  :config
  (fset 'jco/align-last-eq
        [?\C-u ?\C-x ?a ?r ?= ?\[ ?^ ?= ?\] ?* ?$ return return return ?n])
  ;; (evil-leader/set-key "q a" 'jco/align-last-eq)

  (fset 'jco/haskell-list-to-multiline
        [?0 ?f ?\[ ?c ?s ?\] ?\[ ?: ?s ?/ ?, ?  ?/ ?\\ ?n ?, ?  ?/ ?g return
            ?\C-o])
  ;; (evil-leader/set-key "q l" 'jco/haskell-list-to-multiline)

  (let* ((paths (split-string (shell-command-to-string
                               "stack path --local-install-root 2> /dev/null")
                              path-separator))
         (bin-path (concat (string-trim-right (car paths)) "/bin")))
    (setenv "PATH" (concat bin-path ":" (getenv "PATH")))
    (add-to-list 'exec-path bin-path))

  (subword-mode -1)
  ;; (setq flycheck-check-syntax-automatically '(save new-line))

  (flycheck-mode -1)
  (setq haskell-interactive-popup-errors nil)
  (setq haskell-process-auto-import-loaded-modules t)
  (setq haskell-process-log t)
  (setq haskell-process-suggest-remove-import-lines t)
  (setq haskell-process-type 'auto)

  ;; (setq haskell-tags-on-save t) ;; doesn't work for Fish
  ;; (haskell-doc-mode)

  ;; (setq tab-stop-list
  ;;       (loop for i from 0 upto 120 by 4 collect i))

  (setq yas-indent-line 'fixed))

(use-package ebal
  :config
  (setq ebal-operation-mode 'stack)
  (add-hook 'haskell-mode-hook
            (lambda ()
              (evil-leader/set-key "e i" 'ebal-init)
              (evil-leader/set-key "e e" 'ebal-execute))))

(add-hook 'haskell-mode-hook
          (lambda ()
            (smartparens-mode)

            ;; Don't use intero mode in org snippets.
            (when (and (not (eq system-type 'windows-nt))
                       (not (s-contains? "org-src" (buffer-name)))
                       (not (s-contains? "*temp" (buffer-name))))
              (intero-mode))

            (setq evil-shift-width 4)

            (evil-leader/set-key "x s" 'haskell-sort-imports)

            (bind-keys :map haskell-mode-map
                       ("<f8>" . haskell-navigate-imports)
                       ("C-c C-a" . haskell-align-imports)
                       ("C-c C-l" . haskell-process-load-file)
                       ("C-c C-z" . haskell-interactive-switch)
                       ("C-c C-n C-t" . haskell-process-do-type)
                       ("C-c C-n C-i" . haskell-process-do-info)
                       ("C-c C-n C-c" . haskell-process-cabal-build)
                       ("C-c C-n c" . haskell-process-cabal)
                       ("C-c C-o" . haskell-compile)
                       ("C-c C-k" . haskell-interactive-mode-clear))

            (bind-keys :map haskell-cabal-mode-map
                       ("C-c C-z" . haskell-interactive-switch)
                       ("C-c C-k" . haskell-interactive-mode-clear)
                       ("C-c C-c" . haskell-process-cabal-build)
                       ("C-c c" . haskell-process-cabal)
                       ("C-c C-o" . haskell-compile))))

(provide 'init-haskell)

;;; init-haskell.el ends here
