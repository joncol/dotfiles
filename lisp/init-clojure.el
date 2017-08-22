;;; #init-clojure.el --- Clojure config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(require 'init-lisp-common)

(use-package clj-refactor
  :diminish clj-refactor-mode
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
                  cider-classpath-mode)
                evil-motion-state-modes)))

(use-package clojure-snippets)

(use-package kibit-helper)

(add-hook 'clojure-mode-hook
          (lambda ()
            (init-lisp-common)
            (setq-local evil-move-beyond-eol t)
            (setq cider-prompt-for-symbol nil)

            (diminish 'cider-mode)

            (defadvice cider-create-grimoire-buffer
                (after grimoire-buffer-after activate compile)
              "Enables closing the Grimoire buffer with q, for instance."

              (with-current-buffer "*cider-grimoire*"
                (evil-motion-state)))

            (put-clojure-indent 'GET 2)
            (put-clojure-indent 'POST 2)
            (put-clojure-indent 'PUT 2)

            (evil-leader/set-key "h d" 'cider-doc)
            (evil-leader/set-key "h g" 'cider-grimoire)
            (evil-leader/set-key "h G" 'cider-grimoire-web)
            (evil-leader/set-key "h n" 'cider-browse-ns)))

(add-hook 'cider-browse-ns-mode-hook
          (lambda ()
            ;; For some reason, `windmove-default-keybindings' doesn't work.
            (bind-keys :map cider-browse-ns-mode-map
                       ("C-w h"   . windmove-left)
                       ("C-w j"   . windmove-down)
                       ("C-w k"   . windmove-up)
                       ("C-w l"   . windmove-right)
                       ("C-w C-h" . windmove-left)
                       ("C-w C-j" . windmove-down)
                       ("C-w C-k" . windmove-up)
                       ("C-w C-l" . windmove-right))))

(add-hook 'cider-test-report-mode-hook
          (lambda ()
            ;; For some reason, `windmove-default-keybindings' doesn't work.
            (bind-keys :map cider-test-report-mode-map
                       ("C-w h"   . windmove-left)
                       ("C-w j"   . windmove-down)
                       ("C-w k"   . windmove-up)
                       ("C-w l"   . windmove-right)
                       ("C-w C-h" . windmove-left)
                       ("C-w C-j" . windmove-down)
                       ("C-w C-k" . windmove-up)
                       ("C-w C-l" . windmove-right))))

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
