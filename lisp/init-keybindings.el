(defun jco/define-bindings (keymap binding-alist)
  "Define keys for KEYMAP given a BINDING-ALIST"
  (dolist (p binding-alist)
    (let ((key (car p))
          (command (cdr p)))
      (define-key keymap (kbd key) command))))

(provide 'init-keybindings)
