;;; #init-utils.el --- Utils config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(defun jco/yank-current-filename ()
  "Yank the filename of the current buffer to the kill ring."
  (interactive)
  (kill-new (buffer-file-name)))

(defun jco/insert-current-filename (arg)
  "Insert filename of the current buffer at point.
If ARG is given, only the basename (no path and no extension) of the file is
inserted."
  (interactive "P")
  (insert (if arg
              (file-name-base (buffer-file-name))
            (buffer-file-name))))

(defun jco/insert-date (arg)
  "Insert date at current point, in format 2016-11-23.
If ARG is given, dots are used instead of dashes."
  (interactive "P")
  (insert (if arg
              (format-time-string "%d.%m.%Y")
            (format-time-string "%Y-%m-%d"))))

(defun jco/insert-timestamp (arg)
  "Insert timestamp at current point.
The format of the timestamp is \"2021-03-03 Wed 14:31\". If ARG
is given, the format of the timestamp is 2016-11-23T00:00:00 (in
accordance with ISO 8601)."
  (interactive "P")
  (insert (if arg
              (format-time-string "%Y-%m-%dT%H:%M:%S")
            (format-time-string "%Y-%m-%d %a %H:%M"))))

(defun jco/json-lint ()
  "Pretty format JSON."
  (interactive)
  (save-restriction
    (widen)
    (shell-command-on-region (point-min) (point-max) "python -m json.tool"
                             t t)))


(global-set-key (kbd "<f12>") (lambda ()
                                (interactive)
                                (message "Current major mode: %s" major-mode)))

(provide 'init-utils)

;;; init-utils.el ends here
