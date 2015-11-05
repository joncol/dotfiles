(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq jco/init-errors nil)

(require 'init-bootstrap)
(jco/safe-load-init-files)

(message "==============================")
(message (if jco/init-errors
             (mapconcat #'identity jco/init-errors "\n")
           "Emacs initialized successfully"))
(message "==============================")
