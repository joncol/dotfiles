;;; #init-utils.el --- Utils config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(defun yank-current-filename ()
  "Yank the name of the current file to the kill ring."
  (interactive)
  (kill-new (buffer-file-name)))

(global-set-key (kbd "S-<f6>") 'yank-current-filename)

(let ((init-file (concat user-emacs-directory "init.el")))
  (global-set-key (kbd "<f9>")
                  (lambda ()
                    (interactive)
                    (find-file init-file)))

  (global-set-key (kbd "S-<f9>")
                  (lambda ()
                    (interactive)
                    (load-file init-file))))

(global-set-key (kbd "<f12>") (lambda ()
                                (interactive)
                                (message "Current major mode: %s" major-mode)))

(global-set-key (kbd "C-c t f") 'toggle-frame-fullscreen)

(provide 'init-utils)

;;; init-utils.el ends here
