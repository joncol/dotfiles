;;; #init-utils.el --- Utils config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(defun jco/yank-current-filename ()
  "Yank the filename of the current buffer to the kill ring."
  (interactive)
  (kill-new (buffer-file-name)))

(global-set-key (kbd "S-<f6>") 'jco/yank-current-filename)

(let ((init-file (concat user-emacs-directory "init.el")))
  (global-set-key (kbd "<f9>")
                  (lambda ()
                    (interactive)
                    (find-file init-file)))
(defun jco/insert-date (arg)
  "Insert date at current point, in format 2016-11-23.
If ARG is given, dots are used instead of dashes."
  (interactive "P")
  (insert (if arg
              (format-time-string "%d.%m.%Y")
            (format-time-string "%Y-%m-%d"))))

(defun jco/insert-timestamp (arg)
  "Insert timestamp at current point.
The format of the timestamp is 00:00:00.
If ARG is given, the date is also inserted, and the format of the
timestamp is 2016-11-23T00:00:00 (in accordance with ISO 8601)."
  (interactive "P")
  (insert (if arg
              (format-time-string "%Y-%m-%dT%H:%M:%S")
            (format-time-string "%H:%M:%S"))))

(defun jco/json-lint ()
  "Pretty format JSON."
  (interactive)
  (save-restriction
    (widen)
    (shell-command-on-region (point-min) (point-max) "python -m json.tool"
                             t t)))

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
