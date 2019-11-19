;;; #init-clojure.el --- Clojure config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(require 'init-lisp-common)

(defun my-switch-to-repl ()
  "Switch to REPL buffer and move the window to the bottom."
  (interactive)
  (cider-switch-to-repl-buffer)
  (jco/move-window-to-bottom))

(use-package cider
  :defer t
  :bind (:map cider-mode-map
         ("C-c C-z" . my-switch-to-repl)
         :map clojure-mode-map
         ("M-." . cider-find-dwim))
  :config
  (setq cider-show-error-buffer nil))

(use-package clj-refactor
  :after clojure-mode
  :config
  (setq cljr-warn-on-eval nil)
  (setq cljr-auto-clean-ns nil)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode)
              (cljr-add-keybindings-with-prefix "C-c C-m")))

  (setq evil-motion-state-modes
        (append '(cider-docview-mode
                  cider-popup-buffer-mode
                  cider-stacktrace-mode
                  cider-inspector-mode
                  cider-classpath-mode)
                evil-motion-state-modes)))

(use-package flycheck-clj-kondo)

(use-package kibit-helper)

(defun nrepl-reset ()
  "Helper function to call the (Reloaded workflow) reset function."
  (interactive)
  (set-buffer (cider-current-repl-buffer))
  (goto-char (point-max))
  (insert "(reset)")
  (cider-repl-return))

(defun point-at-pos-rel-line-offset (pos rel-line-offset)
  "Return position of point at POS with REL-LINE-OFFSET relative line offset."
  (save-excursion
    (goto-char pos)
    (forward-line rel-line-offset)
    (point)))

(defun disassemble-clojure-fn ()
  "Helper function to disassemble a Clojure function.
Opens a new buffer with the result."
  (interactive)
  (let* ((fn-name  (read-string "Disassemble Clojure function: "
                                (thing-at-point 'symbol t)))
         (buf-name (concat fn-name "-disassembly")))
    (set-buffer (cider-current-repl-buffer))
    (goto-char (point-max))
    (insert "(use 'no.disassemble)")
    (cider-repl-return)
    (sleep-for 0 100)
    (goto-char (point-max))
    (insert (concat "(println (disassemble " fn-name "))"))
    (save-excursion
      (cider-repl-return))
    (sleep-for 0 100)
    (forward-line)
    (if (not (re-search-forward "CompilerException" (line-end-position) t))
        (progn (copy-to-buffer buf-name (point)
                               (point-at-pos-rel-line-offset (point-max) -1))
               (goto-char (point-max))
               (pop-to-buffer buf-name)
               (delete-trailing-whitespace)
               (java-mode))
      (progn
        (goto-char (point-max))
        (message (concat "No function named '" fn-name "' found"))))))

(add-hook 'clojure-mode-hook
          (lambda ()
            (init-lisp-common)
            (setq-local evil-move-beyond-eol t)
            (setq cider-prompt-for-symbol nil)

            (modify-syntax-entries)

            (cider-auto-test-mode)

            (define-key clojure-mode-map (kbd "M-;") #'jco/lisp-comment-dwim)

            (defadvice cider-create-grimoire-buffer
                (after grimoire-buffer-after activate compile)
              "Enables closing the Grimoire buffer with q, for instance."

              (with-current-buffer "*cider-grimoire*"
                (evil-motion-state)))

            (put-clojure-indent 'GET 2)
            (put-clojure-indent 'POST 2)
            (put-clojure-indent 'PUT 2)
            (put-clojure-indent 'defstate nil)
            (put-clojure-indent 'try* 0)

            ;; Indentation for re-frame
            (put-clojure-indent 'reg-cofx 0)
            (put-clojure-indent 'reg-event-ctx 0)
            (put-clojure-indent 'reg-event-db 0)
            (put-clojure-indent 'reg-event-fx 0)
            (put-clojure-indent 'reg-fx 0)
            (put-clojure-indent 'reg-sub 0)
            (put-clojure-indent 'reg-sub-raw 0)
            (put-clojure-indent '->interceptor 0)
            (put-clojure-indent 'fn-traced 1)

            (put-clojure-indent 'extend-freeze 2)
            (put-clojure-indent 'extend-thaw 1)

            ;; Indentation for duct
            (put-clojure-indent 'context 2)

            (put-clojure-indent 'wcar 1)

            (define-clojure-indent
              (alet 'defun)
              (mlet 'defun))

            (add-to-list 'clojure-align-binding-forms "m/mlet")
            (add-to-list 'clojure-align-binding-forms "m/alet")
            (add-to-list 'clojure-align-binding-forms "with-disposable")

            (dolist (m (list 'cider-repl-mode
                             'cider-test-report-mode-hook
                             'clojure-mode))
              (dolist (kv '(("h d" . cider-doc)
                            ("h g" . cider-grimoire)
                            ("h d" . cider-doc)
                            ("h g" . cider-grimoire)
                            ("h G" . cider-grimoire-web)
                            ("h n" . cider-browse-ns)
                            ("t c" . cider-test-clear-highlights)
                            ("t t" . cider-test-run-test)
                            ("t n" . cider-test-run-ns-tests)
                            ("t p" . cider-test-run-project-tests)
                            ("t r" . cider-test-rerun-test)
                            ("t f" . cider-test-rerun-failed-tests)
                            ("x d" . disassemble-clojure-fn)
                            ("x r" . nrepl-reset)
                            ("x e" . cider-pprint-eval-last-sexp-to-repl)))
                (evil-leader/set-key-for-mode m (car kv) (cdr kv))))))

(add-hook 'nrepl-connected-hook #'jco/move-window-to-bottom)

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
                       ("<tab>"     . forward-button)
                       ("<backtab>" . backward-button)
                       ("TAB"       . forward-button)
                       ("C-w h"     . windmove-left)
                       ("C-w j"     . windmove-down)
                       ("C-w k"     . windmove-up)
                       ("C-w l"     . windmove-right)
                       ("C-w C-h"   . windmove-left)
                       ("C-w C-j"   . windmove-down)
                       ("C-w C-k"   . windmove-up)
                       ("C-w C-l"   . windmove-right))))

(defun modify-syntax-entries ()
  "Do not treat valid identifier symbols as word separators."
  (modify-syntax-entry ?- "w")
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?< "w")
  (modify-syntax-entry ?> "w")
  (modify-syntax-entry ?? "w")
  (modify-syntax-entry ?! "w")
  (modify-syntax-entry ?* "w"))

(add-hook 'cider-repl-mode-hook
          (lambda ()
            (modify-syntax-entries)
            (smartparens-mode)
            (turn-off-fci-mode)))

(add-hook 'cider--debug-mode-hook
          (lambda ()
            (evil-make-overriding-map cider--debug-mode-map 'normal)
            (evil-normalize-keymaps)))

(provide 'init-clojure)

;;; init-clojure.el ends here
