(defun vim ()
  "Open current buffer in Vim."
  (interactive)
  (start-process "gvim" nil
                 "gvim"
                 (format "+%d" (line-number-at-pos))
                 (buffer-file-name)))

(provide 'init-ext-apps)
