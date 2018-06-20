;;; init-org.el --- Org Mode config -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defun jco/org-inline-css-hook (exporter)
  "Fix colors of snippets when EXPORTER is 'html.
Insert custom inline css to automatically set the foreground and background of
code, to the current theme's colors."
  (when (eq exporter 'html)
    (let* ((my-pre-bg (face-background 'default))
           (my-pre-fg (face-foreground 'default)))
      (setq
       org-html-head-extra
       (concat
        org-html-head-extra
        (format (concat "<style type=\"text/css\">\n pre.src "
                        "{background-color: %s; color: %s;}</style>\n")
                my-pre-bg my-pre-fg))))))

(defun jco/org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.
PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun jco/org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(cl-defun jco/add-youtube-link-type (name &optional (url-params nil))
  "Add org link type for embedding YouTube links in org-mode."
  (let ((yt-iframe-format
         (concat "<iframe width=\"560\""
                 " height=\"315\""
                 " src=\"https://www.youtube.com/embed/%s?rel=0"
                 url-params
                 "\""
                 " frameborder=\"0\""
                 " allowfullscreen>%s</iframe>")))
    (org-add-link-type name
                       (lambda (handle)
                         (browse-url
                          (concat "https://www.youtube.com/embed/"
                                  handle)))
                       (lambda (path desc backend)
                         (cl-case backend
                           (html (format yt-iframe-format
                                         path (or desc "")))
                           (latex (format "\href{%s}{%s}"
                                          path
                                          (or desc "video"))))))))

(use-package ox-reveal
  :after org)

(defun jco/ensure-todo-org-header ()
  "If the current buffer is empty, insert an org header."
  (when (zerop (buffer-size))
    (insert (concat "#+SEQ_TODO: TODO(t) IN-PROGRESS(i) DONE(d)\n"
                    "#+STARTUP: showall\n\n"))))

(defun jco/goto-current-project-todo-org (headline)
  "Go to project's todo.org, section: HEADLINE."
  (set-buffer (org-capture-target-buffer (concat (projectile-project-root)
                                                 "todo.org")))
  (org-capture-put-target-region-and-position)
  (widen)
  (goto-char (point-min))
  (jco/ensure-todo-org-header)
  (if (re-search-forward (format org-complex-heading-regexp-format
                                 (regexp-quote headline))
                         nil t)
      (beginning-of-line)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "* " headline "\n")
    (beginning-of-line 0)))

(use-package org
  :defer t
  :ensure org-plus-contrib
  :init
  (setq org-directory "~/org")
  (setq org-capture-templates
        '(("t" "Task" entry (file+headline "work.org" "_Incoming")
           "* TODO %^{Description}\n%?\n  :LOGBOOK:\n  - Added: %U\n  :END:\n"
           :empty-lines-before 0)
          ("p" "Project TODO" entry
           (function (lambda () (jco/goto-current-project-todo-org "Todos")))
           "* TODO %^{Description}\n%?\n  :LOGBOOK:\n  - Added: %U\n  :END:\n"
           :empty-lines-before 0)
          ("n" "Note" entry (file+headline "notes.org" "Notes")
           "* %^{Description}\n%?\n  :LOGBOOK:\n  - Added: %U\n  :END:\n"
           :empty-lines-before 0)
          ("a" "Appointment" entry (file  "~/Sync/emacs/gcal_zimpler.org" )
           "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")))
  :config
  (setq org-src-fontify-natively t)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (setq org-log-done t)
  (setq org-default-notes-file "notes.org")
  (setq org-reveal-hlevel 2)
  (setq org-todo-keyword-faces
        '(("TODO" . "deep pink")
          ("IN-PROGRESS" . "orange")
          ("NEXT" . "green2")
          ("WAITING" . "purple")
          ("MAYBE" . "gray60")))
  (setq org-agenda-files (concat org-directory "/agenda-files"))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 9)
                             ("notes.org" :maxlevel . 9)))
  (setq org-use-fast-todo-selection t)
  (setq org-log-into-drawer t)
  (setq org-enforce-todo-dependencies t)
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-agenda-custom-commands
        '(("d" "Daily agenda view"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header
                    "High-priority unfinished tasks:")))
            (agenda "" ((org-agenda-ndays 1)))
            (tags-todo "work"
                       ((org-agenda-skip-function
                         '(or (jco/org-skip-subtree-if-habit)
                              (jco/org-skip-subtree-if-priority ?A)
                              (org-agenda-skip-if nil '(scheduled deadline))))
                        (org-agenda-overriding-header
                         "All normal priority tasks, tagged with `work':"))))
           ((org-agenda-compact-blocks nil)
            (org-agenda-files '("~/org/work.org"))))))
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-show-all-today t)
  (setq org-habit-show-habits-only-for-today t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)
     (dot . t)
     (latex . t)
     (plantuml . t)))
  (setq org-confirm-babel-evaluate nil)
  (if (eq system-type 'windows-nt)
      (setq org-ditaa-jar-path "c:/tools/misc/ditaa.jar"
            org-plantuml-jar-path "c:/tools/misc/plantuml.jar")
    (setq org-ditaa-jar-path "/usr/local/bin/ditaa.jar"
          org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar"))
  (require 'ox-latex)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (add-to-list 'org-latex-inputenc-alist '("utf8" . "utf8x"))
  (setq org-latex-default-packages-alist
        (cons '("mathletters" "ucs" nil)
              org-latex-default-packages-alist))
  (setq org-latex-listings 'minted)
  (setq org-latex-custom-lang-environments
        '((emacs-lisp "common-lispcode")))
  (setq org-latex-minted-options
        '(("frame" "lines")
          ("fontsize" "\\normalsize")
          ;; ("fontsize" "\\scriptsize")
          ("mathescape" "")
          ("samepage" "")
          ("xrightmargin" "0.5cm")
          ("xleftmargin"  "0.5cm")))
  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction=nonstopmode -output-directory=%o %f"
          "pdflatex -shell-escape -interaction=nonstopmode -output-directory=%o %f"
          "pdflatex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))
  (setq org-latex-table-caption-above nil)
  (setq org-latex-default-figure-position "!htb")
  (setq org-mobile-directory (concat org-directory "/mobile"))
  (setq org-mobile-inbox-for-pull (concat org-directory "/index.org"))
  (setq org-mobile-force-id-on-agenda-items nil)
  (load-library "ox-reveal")
  (auto-fill-mode)
  (global-unset-key (kbd "C-x C-v"))
  (jco/define-bindings org-mode-map
                       '(("<f5>" . (lambda ()
                                     (interactive)
                                     (org-remove-inline-images)
                                     (org-ctrl-c-ctrl-c)
                                     (org-display-inline-images)))
                         ("M-o" . helm-org-in-buffer-headings)))
  (require 'org-agenda)
  (bind-keys :map org-agenda-mode-map
             ("j"       . org-agenda-next-item)
             ("k"       . org-agenda-previous-item)
             ("C-w h"   . windmove-left)
             ("C-w j"   . windmove-down)
             ("C-w k"   . windmove-up)
             ("C-w l"   . windmove-right)
             ("C-w C-h" . windmove-left)
             ("C-w C-j" . windmove-down)
             ("C-w C-k" . windmove-up)
             ("C-w C-l" . windmove-right))
  (jco/add-youtube-link-type "yt")
  (jco/add-youtube-link-type "ytnc" "&controls=0")
  (define-key org-mode-map (kbd "M-o") 'ace-link-org)
  (setq org-hide-emphasis-markers t)
  (font-lock-add-keywords
   'org-mode
   '(("^ +\\([-*]\\) "
      (0 (prog1 () (compose-region (match-beginning 1)
                                   (match-end 1) "•"))))))
  (setq org-clock-persist 'history)
  (add-hook 'org-mode-hook 'org-clock-persistence-insinuate)
  (add-hook 'org-export-before-processing-hook 'jco/org-inline-css-hook)
  (add-hook 'message-mode-hook 'turn-on-orgstruct++)
  (require 'ob-clojure))

(jco/define-bindings global-map
                     '(("C-c a"   . org-agenda)
                       ("C-c c"   . org-capture)
                       ("C-c l"   . org-store-link)
                       ("C-c M-w" . org-copy)
                       ("C-c C-w" . org-refile)))


(defmacro jco/find-org-file (filename &optional dir)
  "Open file FILENAME in the directory DIR (default: `org-directory')."
  `(lambda ()
     (interactive)
     (find-file (concat (or ,dir org-directory) "/" ,filename))
     (jco/ensure-todo-org-header)))

(evil-leader/set-key "o g" (jco/find-org-file "gtd.org"))
(evil-leader/set-key "o n" (jco/find-org-file "notes.org"))
(evil-leader/set-key "o r" (jco/find-org-file "reading.org"))
(evil-leader/set-key "o w" (jco/find-org-file "work.org"))
(evil-leader/set-key "o p" (jco/find-org-file "todo.org"
                                              (projectile-project-root)))

(provide 'init-org)

;;; init-org.el ends here
