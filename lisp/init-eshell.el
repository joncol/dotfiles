(defun create-scm-string (type branch)
  "Create a string to be shown in prompt. TYPE is either \"git\" or \"hg\" and
BRANCH is the branch name."
  (propertize (concat "[" type ":"
                      (if (not (string-empty-p branch))
                          branch
                        "no branch")
                      "] ")
              'face `(:foreground "#f62459")))

(defun curr-dir-scm-branch-string (dir)
  "Return current git branch as a string, or the empty string if DIR is not in a
git repo (or the git command is not found)."
  (interactive)
  (cond ((and (eshell-search-path "git")
              (locate-dominating-file dir ".git"))
         (let* ((git-output
                 (shell-command-to-string
                  (concat "git branch | grep '\\*' | sed -e 's/^\\* //'")))
                (git-branch (if (not (string-empty-p git-output))
                                (substring git-output 0 -1)
                              "")))
           (create-scm-string "git" git-branch)))
        ((and (eshell-search-path "hg")
              (locate-dominating-file dir ".hg"))
         (let* ((hg-output
                 (shell-command-to-string (concat "hg branch")))
                (hg-branch (if (not (string-empty-p hg-output))
                               (substring hg-output 0 -1)
                             "")))
           (create-scm-string "hg" hg-branch)))
        (t "")))

(setq eshell-prompt-function
      (lambda ()
        (concat (curr-dir-scm-branch-string (eshell/pwd))
                (abbreviate-file-name (eshell/pwd)) "\n$ ")))

(setq eshell-highlight-prompt t
      eshell-prompt-regexp "\$ ")

(add-hook 'eshell-mode-hook
          (lambda ()
            (set-face-foreground 'eshell-prompt-face "#f39c12")
            (defalias 'ff 'find-file)
            (defalias 'open 'find-file)))

(defun jco/eshell-here ()
  "Open up a new shell in the directory associated with the current buffer's
file. The eshell is renamed to match that directory to make multiple eshell
windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(provide 'init-eshell)
