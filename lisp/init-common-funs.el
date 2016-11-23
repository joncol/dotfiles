(defun jco/at-office-p (&optional print-message)
  (interactive "p")
  (let ((at-office (member system-name '("JCO-LAPTOP" "mbp.local"
                                         "debian.orzone.local"))))
    (if print-message
        (message (if at-office
                     "You're at the office"
                   "You're not at the office"))
      at-office)))

(defun jco/read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

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

(defun jco/common-prog ()
  (ethan-wspace-mode)
  (whitespace-mode)
  (rainbow-delimiters-mode)
  (modify-syntax-entry ?_ "w") ;; do not treat "_" as a word separator
  (local-set-key (kbd "C-c p s a") 'helm-ag-project-root)
  (fci-mode))

(defun jco/insert-date (arg)
  "Insert date at current point, in format 2016-11-23. If a prefix argument is
given, dots are used instead of dashes."
  (interactive "P")
  (insert (if arg
              (format-time-string "%d.%m.%Y")
            (format-time-string "%Y-%m-%d"))))

(defun jco/insert-timestamp (arg)
  "Insert timestamp at current point. The format of the timestamp is 00:00:00.
If a prefix argument is given, the date is also inserted, and the format of the
timestamp is 2016-11-23T00:00:00 (in accordance with ISO 8601)."
  (interactive "P")
  (insert (if arg
              (format-time-string "%Y-%m-%dT%H:%M:%S")
            (format-time-string "%H:%M:%S"))))

(global-set-key (kbd "C-c i d") 'jco/insert-date)
(global-set-key (kbd "C-c i t") 'jco/insert-timestamp)

(defun jco/json-lint ()
  "Pretty format JSON."
  (interactive)
  (save-restriction
    (widen)
    (shell-command-on-region (point-min) (point-max) "python -m json.tool"
                             t t)))

(defun jco/underline-line (&optional char)
  "Underline the current line with a character (\"-\" is the default)."
  (interactive)
  (let ((line-length (jco/get-line-length)))
    (end-of-line)
    (insert (concat "\n" (make-string line-length (or char ?-))))
    (beginning-of-line)))

(global-set-key (kbd "<f7>") 'jco/underline-line)
(global-set-key (kbd "<S-f7>") (lambda () (interactive) (jco/underline-line ?=)))

(defun jco/get-line-length (&optional print-message)
  "Get the length of the current line."
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (let ((line-start-pos (point)))
      (end-of-line)
      (let ((line-length (- (point) line-start-pos)))
        (when print-message (message (format "Current line length: %d"
                                             line-length)))
        line-length))))

(defun jco/capitalize-first-char (&optional string)
  "Capitalize the first characer of STRING."
  (when (and string (> (length string) 0))
   (let ((first-char (substring string 0 1))
         (rest-str (substring string 1)))
     (concat (capitalize first-char) rest-str))))

(defun jco/camel-case-to-sentence (text)
  "Convert `helloWorld` to `Hello world`."
  (interactive)
  (let* ((snake (string-inflection-underscore-function text))
         (words (replace-regexp-in-string "_" " " snake)))
    (jco/capitalize-first-char words)))

(defun jco/class-name ()
  "Return the class name corresponding to the name of the current buffer."
  (interactive)
  (let* ((base-name (file-name-base buffer-file-name)))
    (string-inflection-camelcase-function base-name)))

(defun jco/insert-class-name ()
  "Insert the class name corresponding to the name of the current buffer."
  (interactive)
  (insert (jco/class-name)))

(defun jco/irc-account ()
  "Return a cons cell of username and password."
  (if (file-exists-p "~/.irc")
    (let ((lines (jco/read-lines "~/.irc")))
      (cons (car lines) (cadr lines)))
    '("" . "")))

(defun jco/update-config ()
  (shell-process-pushd user-emacs-directory)
  (monky-fetch)
  (shell-process-popd "1"))

(provide 'init-common-funs)
