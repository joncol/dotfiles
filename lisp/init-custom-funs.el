(defun jco/at-office-p ()
  (member system-name '("orz-lap01" "mbp.local")))

(defun jco/define-bindings (keymap binding-alist)
  "Define keys for KEYMAP given a BINDING-ALIST."
  (dolist (p binding-alist)
    (let ((key (car p))
          (command (cdr p)))
      (define-key keymap (kbd key) command))))

(defun jco/move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another."
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))

(defun jco/common-prog ()
  (rainbow-delimiters-mode)
  (modify-syntax-entry ?_ "w") ;; do not treat "_" as a word separator
  (local-set-key (kbd "C-c p s a") 'helm-ag-project-root)
  (fci-mode))

(defun jco/insert-date (arg)
  "Insert date at current point."
  (insert (if arg
              (format-time-string "%d.%m.%Y")
            (format-time-string "%Y-%m-%d"))))

(defun jco/insert-timestamp (arg)
  "Insert timestamp at current point."
  (insert (if arg
              (format-time-string "%Y-%m-%dT%H:%M:%S")
            (format-time-string "%H:%M:%S"))))

(global-set-key (kbd "C-c i d")
                (lambda () (interactive) (insert-date nil)))

(global-set-key (kbd "C-c i T")
                (lambda () (interactive) (insert-timestamp t)))

(global-set-key (kbd "C-c i t")
                (lambda () (interactive) (insert-timestamp nil)))

(provide 'init-custom-funs)
