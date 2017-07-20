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
              (cljr-add-keybindings-with-prefix "C-c C-m"))))

(add-hook 'clojure-mode-hook
          (lambda ()
            (init-lisp-common)
            (setq-local evil-move-beyond-eol t)))

(setq evil-motion-state-modes
      (append '(cider-docview-mode
                cider-popup-buffer-mode
                cider-stacktrace-mode)
              evil-motion-state-modes))

(provide 'init-clojure)

;;; init-clojure.el ends here
