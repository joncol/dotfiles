(setq org-src-fontify-natively t)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t)
(setq org-directory "~/org")
(setq org-default-notes-file "notes.org")
(jco/define-bindings global-map '(("C-c a" . org-agenda)
                                  ("C-c c" . org-capture)
                                  ("C-c l" . org-store-link)
                                  ("C-c M-w" . org-copy)
                                  ("C-c C-w" . org-refile)
                                  ("C-c g" . (lambda ()
                                               (interactive)
                                               (find-file
                                                (concat org-directory
                                                        "/gtd.org"))))
                                  ("C-c n" . (lambda ()
                                               (interactive)
                                               (find-file
                                                (concat org-directory
                                                        "/notes.org"))))
                                  ("C-c w" . (lambda ()
                                               (interactive)
                                               (find-file
                                                (concat org-directory
                                                        "/work.org"))))))
(setq org-reveal-hlevel 2)
(setq org-todo-keyword-faces
      '(("TODO" . "deep pink")
        ("IN_PROGRESS" . "orange")
        ("NEXT" . "orange")
        ("WAITING" . "purple")
        ("MAYBE" . "gray60")))
(setq org-agenda-files (concat org-directory "/agenda-files"))
(setq org-refile-targets '((org-agenda-files :level . 1)
                           ("notes.org" :maxlevel . 9)))
;; (setq org-outline-path-complete-in-steps nil)
;; (setq org-refile-use-outline-path t)
(setq org-use-fast-todo-selection t)
(setq org-capture-templates
      '(("t" "Task" entry (file+headline "work.org" "Tasks")
         "* TODO %^{Description}\n%?\n  :LOGBOOK:\n  - Added: %U\n  :END:\n")
        ("n" "Note" entry (file+headline "notes.org" "Notes")
         "* %^{Description}\n%?\n  :LOGBOOK:\n  - Added: %U\n  :END:\n")))

(setq org-log-into-drawer t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)
   (dot . t)
   (plantuml . t)))

(setq org-confirm-babel-evaluate nil)
(if (eq system-type 'windows-nt)
    (setq org-ditaa-jar-path "c:/tools/misc/ditaa.jar"
          org-plantuml-jar-path "c:/tools/misc/plantuml.jar")
  (setq org-ditaa-jar-path "/usr/local/bin/ditaa.jar"
        org-plantuml-jar-path "/usr/local/bin/plantuml.jar"))

(setq org-latex-listings 'minted)
(setq org-latex-custom-lang-environments
      '((emacs-lisp "common-lispcode")))
(setq org-latex-minted-options
      '(("frame" "lines")
        ("fontsize" "\\scriptsize")
        ("linenos" "")))
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction=nonstopmode -output-directory=%o %f"
        "pdflatex -shell-escape -interaction=nonstopmode -output-directory=%o %f"
        "pdflatex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))

(add-hook 'org-mode-hook
          (lambda ()
            (load-library "ox-reveal")
            (auto-fill-mode)
            (global-unset-key (kbd "C-x C-v"))
            (jco/define-bindings org-mode-map
                                 '(("<f5>" . (lambda ()
                                               (interactive)
                                               (org-remove-inline-images)
                                               (org-ctrl-c-ctrl-c)
                                               (org-display-inline-images)))))

             ;;; Embed youtube links in org-mode

            (cl-defun add-link-type (name &optional (url-params nil))
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

            (add-link-type "yt")
            (add-link-type "ytnc" "&controls=0")))

(provide 'init-org)
