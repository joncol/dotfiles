;;; #init-rotate.el --- Text rotations -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(defvar jco/rotate-text-rotations
  '(("true" "false")
    ("True" "False")
    ("TRUE" "FALSE")
    ("yes" "no")
    ("Yes" "No")
    ("YES" "NO")
    ("before" "after")
    ("Before" "After")
    ("BEFORE" "AFTER")
    ("begin" "end")
    ("Begin" "End")
    ("BEGIN" "END")
    ("width" "height")
    ("Width" "Height")
    ("WIDTH" "HEIGHT")
    ("x" "y")
    ("X" "Y")
    ("in" "out")
    ("In" "Out")
    ("IN" "OUT")
    ("client" "server")
    ("Client" "Server")
    ("CLIENT" "SERVER"))
  "List of text rotation sets.")

(defun rotate-word-at-point ()
  "Rotate word at point based on contents of `jco/rotate-text-rotations'."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word))
        (opoint (point)))
    (when (consp bounds)
      (let ((beg (car bounds))
            (end (copy-marker (cdr bounds))))
        (rotate-region beg end)
        (goto-char (if (> opoint end) end opoint))))))

(defun rotate-region (beg end)
  "Rotate all matches in `jco/rotate-text-rotations' between point and mark."
  (interactive "r")
  (let ((regexp (jco/rotate-convert-rotations-to-regexp
                 jco/rotate-text-rotations))
        (end-mark (copy-marker end)))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward regexp (marker-position end-mark) t)
        (let* ((found (match-string 0))
               (replace (rotate-next found)))
          (replace-match replace))))))

(defun rotate-string (string &optional rotations)
  "Rotate all matches in STRING using associations in ROTATIONS.
If ROTATIONS are not given it defaults to `jco/rotate-text-rotations'."
  (let ((regexp (jco/rotate-convert-rotations-to-regexp
                 (or rotations jco/rotate-text-rotations)))
        (start 0))
    (while (string-match regexp string start)
      (let* ((found (match-string 0 string))
             (replace (rotate-next
                       found
                       (or rotations jco/rotate-text-rotations))))
        (setq start (+ (match-end 0)
                       (- (length replace) (length found))))
        (setq string (replace-match replace nil t string))))
    string))

(defun rotate-next (string &optional rotations)
  "Return the next element after STRING in ROTATIONS."
  (let ((rots (rotate-get-rotations-for
               string
               (or rotations jco/rotate-text-rotations))))
    (if (> (length rots) 1)
        (error (format "Ambiguous rotation for %s" string))
      (if (< (length rots) 1)
          ;; If we get this far, this should not occur:
          (error (format "Unknown rotation for %s" string))
        (let ((occurs-in-rots (member string (car rots))))
          (if (null occurs-in-rots)
              ;; If we get this far, this should *never* occur:
              (error (format "Unknown rotation for %s" string))
            (if (null (cdr occurs-in-rots))
                (caar rots)
              (cadr occurs-in-rots))))))))

(defun rotate-get-rotations-for (string &optional rotations)
  "Return the string rotations for STRING in ROTATIONS."
  (remq nil (mapcar (lambda (rot) (if (member string rot) rot))
                    (or rotations jco/rotate-text-rotations))))

(defun jco/rotate-convert-rotations-to-regexp (rotations)
  (regexp-opt (jco/rotate-flatten-list rotations)))

(defun jco/rotate-flatten-list (list-of-lists)
  "Flatten LIST-OF-LISTS to a single list.
Example:
  (jco/rotate-flatten-list '((a b c) (1 ((2 3)))))
    => (a b c 1 2 3)"
  (if (null list-of-lists)
      list-of-lists
    (if (listp list-of-lists)
        (append (jco/rotate-flatten-list (car list-of-lists))
                (jco/rotate-flatten-list (cdr list-of-lists)))
      (list list-of-lists))))

(provide 'init-rotate)

;;; init-rotate.el ends here
