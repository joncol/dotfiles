(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(defvar jco/init-errors nil
  "If there are any initialization errors, they will be appended to this list.")

(setq evil-want-C-i-jump nil)

(require 'init-bootstrap)
(jco/safe-load-init-files)

(message "==============================")
(message (if jco/init-errors
             (mapconcat #'identity jco/init-errors "\n")
           "Emacs initialized successfully"))
(message "==============================")
