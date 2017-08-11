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
              (cljr-add-keybindings-with-prefix "C-c C-m")
              (setq cljr-warn-on-eval nil)))

  (setq evil-motion-state-modes
        (append '(cider-docview-mode
                  cider-popup-buffer-mode
                  cider-stacktrace-mode
                  cider-inspector-mode
                  cider-classpath-mode
                  cider-test-report-mode)
                evil-motion-state-modes)))

(add-hook 'clojure-mode-hook
          (lambda ()
            (init-lisp-common)
            (setq-local evil-move-beyond-eol t)

            (defadvice cider-create-grimoire-buffer
                (after grimoire-buffer-after activate compile)
              "Enables closing the Grimoire buffer with q, for instance."

              (with-current-buffer "*cider-grimoire*"
                (evil-motion-state)))

            (evil-leader/set-key "h g" 'cider-grimoire)
            (evil-leader/set-key "h G" 'cider-grimoire-web)))

(use-package clojure-snippets)

(add-hook 'cider-repl-mode-hook
          (lambda ()
            ;; Do not treat "-" and "_" as word separators.
            (modify-syntax-entry ?- "w")
            (modify-syntax-entry ?_ "w")))

(add-hook 'cider--debug-mode-hook
          (lambda ()
            (evil-make-overriding-map cider--debug-mode-map 'normal)
            (evil-normalize-keymaps)))

(provide 'init-clojure)

;;; init-clojure.el ends here
