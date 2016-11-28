;;; init.el --- Main startup file
;;; Commentary:
;;
;;; Code:
(package-initialize) ;; Don't delete this. It will be readded automatically.

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq-default flycheck-emacs-lisp-load-path load-path)

(defvar evil-want-C-i-jump nil)

(require 'init-bootstrap)
(jco/safe-load-init-files)

;; Change for different username.
(setq inhibit-startup-echo-area-message "jco")

(message "==============================")
(message (if jco/init-errors
             (mapconcat #'identity jco/init-errors "\n")
           "Emacs initialized successfully"))
(message "==============================")

(if jco/init-errors
    (message "%s" (propertize "There were errors" 'face '(:foreground "red")))
  (message "%s" (propertize "No errors" 'face '(:foreground "green"))))

(provide 'init)

;;; init.el ends here
