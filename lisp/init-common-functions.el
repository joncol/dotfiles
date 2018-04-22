;;; #init-common-functions.el --- Common functions -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(defun apply-ansi-colors ()
  "Apply ANSI colors on region."
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun get-ip-address (&optional dev)
  "Get the IP-address for device DEV (default: eth0)."
  (let ((dev (if dev dev "eth0")))
    (format-network-address (car (network-interface-info dev)) t)))

(defun jco/update-config ()
  "Get the latest config from source control."
  (shell-process-pushd user-emacs-directory)
  (magit-pull "origin/master" nil)
  (shell-process-popd "1"))

(defun jco/at-office-p (&optional print-message)
  "Check whether at the office.
If PRINT-MESSAGE is true, a message will be printed indicating the result."
  (interactive "P")
  (let ((result (member system-name '("jco-debian-zimpler"))))
    (if print-message
        (message (if result
                     "You're at the office"
                   "You're not at the office"))
      result)))

(defun jco/at-digitalocean-p (&optional print-message)
  "Check whether on the DigitalOcean server.
If PRINT-MESSAGE is true, a message will be printed indicating the result."
  (interactive "P")
  (let ((result (string-equal "162.243.220.203" (get-ip-address))))
    (if print-message
        (message (if result
                     "You're on DigitalOcean"
                   "You're not on DigitalOcean"))
      result)))

(defun jco/read-lines (filePath)
  "Return a list of lines of a file at FILEPATH."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun jco/define-bindings (keymap binding-alist)
  "Define keys for KEYMAP given a BINDING-ALIST."
  (dolist (p binding-alist)
    (let ((key (car p))
          (command (cdr p)))
      (define-key keymap (kbd key) command))))

(defun jco/move-key (key keymap-from keymap-to)
  "Move KEY binding from KEYMAP-FROM to KEYMAP-TO."
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))

(defun jco/common-prog ()
  "Common setup for programming modes."
  (ethan-wspace-mode)
  (rainbow-delimiters-mode)
  (rainbow-mode t)
  (modify-syntax-entry ?_ "w") ;; do not treat "_" as a word separator
  (fci-mode))

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
  "Underline the current line with a character CHAR (\"-\" is the default)."
  (interactive)
  (let ((line-length (jco/get-line-length)))
    (end-of-line)
    (insert (concat "\n" (make-string line-length (or char ?-))))
    (beginning-of-line)))

(global-set-key (kbd "<f7>") 'jco/underline-line)
(global-set-key (kbd "<S-f7>") (lambda () (interactive) (jco/underline-line ?=)))

(defun jco/get-line-length (&optional print-message)
  "Get the length of the current line.
If PRINT-MESSAGE is non-nil, print a message"
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

(defun jco/downcase-first-char (string)
  "Downcase the first character of STRING."
  (concat (downcase (substring string 0 1)) (substring string 1)))

(defun jco/cpp-insert-class-name ()
  "Insert the class name corresponding to the name of the current buffer."
  (interactive)
  (insert (jco/cpp-class-name)))

(defun jco/irc-account ()
  "Return a cons cell of username and password."
  (if (file-exists-p "~/.irc")
    (let ((lines (jco/read-lines "~/.irc")))
      (cons (car lines) (cadr lines)))
    '("" . "")))

(defun jco/cmake-target-string ()
  "Get target string for CMake."
  (let ((proj (jco/cmake-project-name)))
    (if (string= proj "src")
        ""
      (concat "--target " proj))))

(defun jco/cmake-project-name ()
  "Get name of CMake project.
Traverses the directory hierarchy upwards and looks for the first
CMakeLists.txt file."
  (let ((dir (locate-dominating-file (buffer-file-name) "CMakeLists.txt")))
    (car (last (f-split dir)))))

(defun jco/vcs-status ()
  "Run either monky-status or magit-status, depending on the kind of repo."
  (interactive)
  (cl-case (projectile-project-vcs)
    (git (magit-status))
    (hg (monky-status))))

(defun jco/vcs-update ()
  "Run either `hg pull -u' or `git pull', depending on the kind of repo."
  (interactive)
  (cl-case (projectile-project-vcs)
    (git (magit-pull "origin/master" nil))
    (hg (monky-hg-command "pull -u"))))

(defun jco/what-face (pos)
  "Determine the face at the point POS."
  (interactive "d")
  (let ((g (global-hl-line-mode)))
    (global-hl-line-mode -1)
    (let ((face (or (get-char-property (point) 'read-face-name)
                    (get-char-property (point) 'face))))
      (if face
          (message "Face: %s" face)
        (message "No face at %d" pos)))
    (global-hl-line-mode g)))

(defun jco/re-seq (regexp string)
  "Get a list of all regex-matches of REGEXP in STRING."
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      (nreverse matches))))

(defun jco/cmake-compile-command ()
  "Return compile command for building a project using CMake."
  (when (and (stringp (buffer-file-name))
             (projectile-project-p))
    (let ((sh (getenv "SHELL"))
          (target (jco/cmake-target-string)))
      (concat
       "cd " (projectile-project-root)
       (cond
        ((s-contains-p "fish" sh)
         (format "_build ;and cmake --build . %s -- -j4" target))
        ((eq system-type 'windows-nt)
         (format "_build && cmake --build . %s" target))
        (t (format
            "_build && cmake --build . %s -- -j4" target)))))))

(defun jco/display-ansi-colors ()
  "Display ansi color codes in buffer."
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun jco/collapse-multiple-empty-lines ()
  "Replace multiple consecutive empty lines with one empty line."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\n\n+" nil t)
      (replace-match "\n\n"))))

(defun jco/tighten-braces ()
  "Fix formatting of braces.
Remove empty lines after opening brace and before closing brace."
  (interactive)
  (save-excursion
    ;; Remove empty line(s) after opening brace.
    (goto-char (point-min))
    (while (re-search-forward "{\n\n+" nil t)
      (replace-match "{\n"))

    ;; Remove empty line(s) before closing brace.
    (goto-char (point-min))
    (while (re-search-forward "\n\n+\\(\\s-*\\)}" nil t)
      (replace-match "\n\\1}"))))

(defun jco/run-process (program &rest args)
  "Start process PROGRAM with arguments ARGS."
  (apply 'start-process program nil program args))

(defun jco/run-on-current-buffer (program &rest args)
  "Start process PROGRAM with arguments ARGS on current buffer.
The filename of the current buffer is passed as the last argument to the process
invokation."
  (apply 'start-process program nil program
         (append args (list (buffer-file-name)))))

(defun jco/vim ()
  "Open current buffer in Vim."
  (interactive)
  (start-process "gvim" nil
                 "gvim"
                 (format "+%d" (line-number-at-pos))
                 (buffer-file-name)))

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defun jco/find-buffers-by-regex (re)
  "Find the first buffer with a name matching RE."
  (seq-filter (lambda(b) (string-match re (buffer-name b))) (buffer-list)))

(defun jco/select-bottom-window ()
  "Select the bottommost window."
  (let ((bottom-window (selected-window))
        window-below)
    (while (setq window-below (window-in-direction 'below bottom-window))
      (setq bottom-window window-below))
    (select-window bottom-window)))

(provide 'init-common-functions)

;;; init-common-functions.el ends here
