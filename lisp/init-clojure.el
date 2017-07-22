;;; #init-clojure.el --- Clojure config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(require 'init-lisp-common)

(use-package clj-refactor
  :config
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode)
              (cljr-add-keybindings-with-prefix "C-c C-m")))

  (setq evil-motion-state-modes
        (append '(cider-docview-mode
                  cider-popup-buffer-mode
                  cider-stacktrace-mode
                  cider-inspector-mode)
                evil-motion-state-modes)))

(add-hook 'clojure-mode-hook
          (lambda ()
            (init-lisp-common)
            (setq-local evil-move-beyond-eol t)))

(use-package clojure-snippets)

(add-hook 'cider-repl-mode-hook
          (lambda ()
            ;; do not treat "-" as a word separator
            (modify-syntax-entry ?- "w")))

(add-hook 'cider--debug-mode-hook
          (lambda ()
            (evil-make-overriding-map cider--debug-mode-map 'normal)
            (evil-normalize-keymaps)))

(provide 'init-clojure)

;;; init-clojure.el ends here
