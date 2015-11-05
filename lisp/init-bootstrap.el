(let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path lisp-dir))

(defvar init-files
  '(init-custom-funs
    init-keybindings
    init-ruby))

(defun jco/safe-require (feature)
  "Safely require FEATURE"
  (condition-case ex
      (require feature)
    ('error (add-to-list 'jco/init-errors
                         (format "[error loading \"%s\"]: %s"
                                 (symbol-name feature) ex)))))

(defun jco/safe-load-init-files ()
  (dolist (file init-files)
    (jco/safe-require file)))

(defun jco/unsafe-load-init-files ()
  (dolist (file init-files)
    (require file)))

(provide 'init-bootstrap)
