;;; #init-clojure.el --- Clojure config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(require 'init-lisp-common)

(global-set-key (kbd "C-c M-s") #'cider-selector)

(defun create-test-report-window (&rest _)
  "Create window to show test report buffer, if one exists.
Place it to the right of the current window. If a window for the test report
buffer already exists, don't create a new one."
  (when-let* ((buf (get-buffer cider-test-report-buffer)))
    (unless (get-buffer-window buf)
      (let ((buffer-window (split-window (selected-window)
                                         (/ (window-width) 2)
                                         'right)))
        (set-window-buffer buffer-window buf)
        (display-buffer-record-window 'window buffer-window buf)
        (set-window-prev-buffers buffer-window nil)
        (select-window buffer-window)))))

(use-package cider
  :defer t
  :bind (:map clojure-mode-map
         ("M-." . cider-find-dwim))
  :config
  (advice-add 'cider-switch-to-repl-buffer :after #'jco/move-window-to-bottom)
  (advice-add 'cider-test-show-report :before #'create-test-report-window)
  (advice-add 'cider-popup-buffer :before #'create-test-report-window)
  (setq cider-repl-display-help-banner nil)
  (setq cider-show-error-buffer nil)
  (setq cider-auto-select-test-report-buffer t)
  (setq cider-test-show-report-on-success nil)
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq cider-jump-to-pop-to-buffer-actions
        '((display-buffer-reuse-window display-buffer-same-window)))
  ;; (setq cider-repl-result-prefix ";; => ")
  )

(use-package cider-eval-sexp-fu
  :after cider)

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

(defun close-repl-window ()
  "Close the current REPL window."
  (cider-switch-to-repl-buffer)
  (delete-window))

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

(defun cljfmt-buffer ()
  "Run `cljfmt --fix' on current buffer, after saving it."
  (interactive)
  (when (or (eq major-mode 'clojure-mode)
            (eq major-mode 'clojurescript-mode))
    (save-buffer)
    (shell-command-to-string (format "cljfmt --fix %s" buffer-file-name))
    (revert-buffer :ignore-auto :noconfirm)))

(add-hook 'clojure-mode-hook
          (lambda ()
            (init-lisp-common)
            (setq-local evil-move-beyond-eol t)
            (setq cider-prompt-for-symbol nil)

            (modify-syntax-entries)

            (cider-auto-test-mode)

            (define-key clojure-mode-map (kbd "M-;") #'jco/lisp-comment-dwim)

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

            (evil-leader/set-key "l" 'cljfmt-buffer)

            (dolist (m (list 'cider-repl-mode
                             'cider-test-report-mode-hook
                             'clojure-mode))
              (dolist (kv '(("h d" . cider-doc)
                            ("h d" . cider-doc)
                            ("h n" . cider-browse-ns)
                            ("h s" . cider-browse-spec-all)
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
            (bind-window-keys cider-browse-ns-mode-map)))

(add-hook 'cider-stacktrace-mode-hook
          (lambda ()
            ;; For some reason, `windmove-default-keybindings' doesn't work.
            (bind-window-keys cider-stacktrace-mode-map)))

(add-hook 'cider-test-report-mode-hook
          (lambda ()
            ;; For some reason, `windmove-default-keybindings' doesn't work.
            (bind-window-keys cider-test-report-mode-map)
            (bind-keys :map cider-test-report-mode-map
              ("<tab>"     . forward-button)
              ("<backtab>" . backward-button)
              ("TAB"       . forward-button))))

(defun modify-syntax-entries ()
  "Do not treat valid identifier symbols as word separators."
  (modify-syntax-entry ?- "w")
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?< "w")
  (modify-syntax-entry ?> "w")
  (modify-syntax-entry ?? "w")
  (modify-syntax-entry ?! "w")
  (modify-syntax-entry ?* "w"))

(add-hook 'cider-mode-hook
          (lambda ()
            (cider-company-enable-fuzzy-completion)
            (advice-add 'cider-quit :before #'close-repl-window)))

(add-hook 'cider-repl-mode-hook
          (lambda ()
            (cider-company-enable-fuzzy-completion)
            (modify-syntax-entries)
            (smartparens-mode)
            (turn-off-fci-mode)))

(add-hook 'cider--debug-mode-hook
          (lambda ()
            (evil-make-overriding-map cider--debug-mode-map 'normal)
            (evil-normalize-keymaps)))

(provide 'init-clojure)

;;; init-clojure.el ends here
