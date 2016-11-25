;;; init.el --- Main startup file
;;; Commentary:
;;
;;; Code:
(package-initialize) ;; Don't delete this. It will be readded automatically.

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defvar evil-want-C-i-jump nil)

(require 'init-bootstrap)
(jco/safe-load-init-files)

(message "==============================")
(message (if jco/init-errors
             (mapconcat #'identity jco/init-errors "\n")
           "Emacs initialized successfully"))
(message "==============================")

(provide 'init)

;;; init.el ends here
