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

(provide 'init-keybindings)
