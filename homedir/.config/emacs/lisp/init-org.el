;;; init-org.el --- Org Mode config -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(setq org-directory "~/org")

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

;; See: https://github.com/emacs-evil/evil-surround/issues/20#issuecomment-471516289
(defmacro define-and-bind-quoted-text-object (name key start-regex end-regex)
  (let ((inner-name (make-symbol (concat "evil-inner-" name)))
        (outer-name (make-symbol (concat "evil-a-" name))))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key #',inner-name)
       (define-key evil-outer-text-objects-map ,key #',outer-name))))

(use-package cha
  :straight (cha :type git :host github :repo "joncol/cha")
  :commands (cha-create-story cha-edit-story)
  :init
  (evil-leader/set-key "x c" #'cha-create-story)
  (evil-leader/set-key "x e" #'cha-edit-story)
  :config
  (setq cha-clubhouse-default-project "Backend")
  (require 'my-secrets (concat user-emacs-directory "lisp/my-secrets.el.gpg")))

(use-package org-bullets
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

(use-package org-cliplink
  :config
  (evil-leader/set-key-for-mode 'org-mode "x l" 'org-cliplink))

(use-package org-download
  :init
  (add-hook 'org-mode-hook
            (lambda ()
              (require 'org-download))))

(use-package org-re-reveal
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
  :custom
  (org-footnote-auto-adjust t)
  (org-M-RET-may-split-line nil)
  :init
  (setq org-return-follows-link t)
  (setq org-startup-indented t)
  (setq org-edit-src-content-indentation 0)
  (setq org-capture-templates
        '(("t" "Task" entry (file+headline "incoming.org" "Incoming tasks")
           "* TODO %^{Description}\n:LOGBOOK:\n- Added: %U\n:END:\n%?\n"
           :empty-lines-before 0)
          ("p" "Project TODO" entry
           (function (lambda () (jco/goto-current-project-todo-org "Todos")))
           "* TODO %^{Description}\n:LOGBOOK:\n- Added: %U\n:END:\n%?\n"
           :empty-lines-before 0)
          ("n" "Note" entry (file+headline "notes.org" "Notes")
           "* %^{Description}\n:LOGBOOK:\n- Added: %U\n:END:\n%?\n"
           :empty-lines-before 0)
          ("a" "Appointment" entry (file "~/Sync/emacs/gcal_zimpler.org")
           "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
          ("w" "Web" entry (file+headline "web.org" "_Incoming")
           "* %:description\n%:initial\n\nSource: %:link\n:LOGBOOK:\n- Added: %U\n:END:\n"
           :empty-lines-before 0)))
  :config
  (evil-leader/set-key-for-mode 'org-mode "z f" 'org-footnote-new)
  (add-hook 'org-capture-mode-hook
            (lambda ()
              (god-local-mode -1)))
  (setq org-startup-truncated nil)
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
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.6))
  (setq org-agenda-files (concat org-directory "/agenda-files"))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 9)
                             ("~/org/notes.org" :maxlevel . 9)
                             ("~/org/reading.org" :maxlevel . 9)))
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
           ((org-agenda-compact-blocks nil)))))
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (display-fill-column-indicator-mode -1)))
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-show-all-today t)
  (setq org-habit-show-habits-only-for-today t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)
     (dot . t)
     (haskell . t)
     (latex . t)
     (octave . t)
     (plantuml . t)
     (python . t)
     (shell . t)
     (sql . t)))
  (setq org-confirm-babel-evaluate nil)
  (if (eq system-type 'windows-nt)
      (setq org-ditaa-jar-path "c:/tools/misc/ditaa.jar"
            org-plantuml-jar-path "c:/tools/misc/plantuml.jar")
    (setq org-ditaa-jar-path "~/.nix-profile/lib/ditaa.jar"
          org-plantuml-jar-path "~/.nix-profile/lib/plantuml.jar"))
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
        '("latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f"))
  ;; (setq org-latex-pdf-process
  ;;       '("pdflatex -shell-escape -interaction=nonstopmode -output-directory=%o %f"
  ;;         "pdflatex -shell-escape -interaction=nonstopmode -output-directory=%o %f"
  ;;         "pdflatex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))
  (setq org-latex-table-caption-above nil)
  (setq org-latex-default-figure-position "!htb")
  (setq org-mobile-directory (concat org-directory "/mobile"))
  (setq org-mobile-inbox-for-pull (concat org-directory "/index.org"))
  (setq org-mobile-force-id-on-agenda-items nil)
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

  (add-hook 'org-mode-hook
            (lambda ()
              (org-clock-persistence-insinuate)
              (turn-on-auto-fill)
              (display-fill-column-indicator-mode -1)
              ;; (flyspell-mode)
              (smartparens-mode -1)
              (evil-leader/set-key "z l" 'org-toggle-link-display)
              (setq company-idle-delay 0.5)
              (define-and-bind-quoted-text-object "tilde" "~" "~" "~")
              (define-and-bind-quoted-text-object "equals" "=" "=" "=")
              (define-and-bind-quoted-text-object "slash" "/" "/" "/")))

  (add-hook 'org-export-before-processing-hook 'jco/org-inline-css-hook)
  (require 'ob-clojure)
  (eval-after-load "org"
    '(require 'ox-gfm nil t))
  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  (add-to-list 'org-latex-classes
               '("extarticle"
                 "\\documentclass[14pt]{extarticle}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; Set default column view headings: Task Total-Time Time-Stamp
  (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA"))

(use-package org-habit-plus
  :straight (org-habit-plus :type git :host github
                            :repo "oddious/org-habit-plus")
  :defer t
  :init
  (add-to-list 'org-modules 'org-habit))

;; Source: https://org-roam.discourse.group/t/creating-an-org-roam-note-from-an-existing-headline/978
(defun org-roam-create-note-from-headline ()
  "Create an Org-roam note from the current headline and jump to it.

Normally, insert the headline’s title using the ’#title:’ file-level property
and delete the Org-mode headline. However, if the current headline has a
Org-mode properties drawer already, keep the headline and don’t insert
‘#+title:'. Org-roam can extract the title from both kinds of notes, but using
‘#+title:’ is a bit cleaner for a short note, which Org-roam encourages."
  (interactive)
  (org-cycle 2)
  (let ((title (nth 4 (org-heading-components)))
        (has-properties (org-get-property-block)))
    (org-cut-subtree)
    (org-roam-find-file title nil nil 'no-confirm)
    (org-paste-subtree)
    (unless has-properties
      (kill-line)
      (while (outline-next-heading)
        (org-promote)))
    (goto-char (point-min))
    (when has-properties
      (kill-line)
      (kill-line))
    (goto-char (point-at-pos-rel-line-offset (point-min) 3))
    (delete-blank-lines)
    (indent-region (point-min) (point-max))))

(use-package org-noter
  :defer 1
  :if (and (not (eq system-type 'windows-nt))
           (display-graphic-p))
  :bind (:map org-noter-doc-mode-map
         (("M-I" . org-noter-insert-note)))
  :config
  (setq org-noter-always-create-frame nil)
  (setq org-noter-hide-other nil)
  (setq org-noter-notes-search-path '("~/org/roam"))
  (org-noter-set-auto-save-last-location t)
  (evil-leader/set-key "z n" 'org-noter))

(use-package org-noter-pdftools
  :after org-noter
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions
            (if toggle-no-questions
                (not org-noter-insert-note-no-questions)
              org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freestyle-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location
                      (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))

  ;; Don't focus PDF after syncing notes.

  (defun org-noter-sync-prev-note ()
    "Go to the location of the previous note, in relation to where the point is.
As such, it will only work when the notes window exists."
    (interactive)
    (org-noter--with-selected-notes-window
     "No notes window exists"
     (let ((org-noter--inhibit-location-change-handler t)
           (contents (org-element-contents (org-noter--parse-root)))
           (current-begin (org-element-property :begin (org-noter--get-containing-heading)))
           previous)
       (when current-begin
         (org-noter--map-ignore-headings-with-doc-file
          contents t
          (when location
            (if (= current-begin (org-element-property :begin headline))
                t
              (setq previous headline)
              nil))))

       (if previous
           (progn
             ;; NOTE(nox): This needs to be manual so we can focus the correct note
             (org-noter--doc-goto-location (org-noter--parse-location-property previous))
             (org-noter--focus-notes-region (org-noter--make-view-info-for-single-note session previous)))
         (user-error "There is no previous note")))))

  (defun org-noter-sync-current-note ()
    "Go the location of the selected note, in relation to where the point is.
As such, it will only work when the notes window exists."
    (interactive)
    (org-noter--with-selected-notes-window
     "No notes window exists"
     (if (string= (org-entry-get nil org-noter-property-doc-file t) (org-noter--session-property-text session))
         (let ((location (org-noter--parse-location-property (org-noter--get-containing-heading))))
           (if location
               (org-noter--doc-goto-location location)
             (user-error "No note selected")))
       (user-error "You are inside a different document"))))

  (defun org-noter-sync-next-note ()
    "Go to the location of the next note, in relation to where the point is.
As such, it will only work when the notes window exists."
    (interactive)
    (org-noter--with-selected-notes-window
     "No notes window exists"
     (let ((org-noter--inhibit-location-change-handler t)
           (contents (org-element-contents (org-noter--parse-root)))
           next)

       (org-noter--map-ignore-headings-with-doc-file
        contents t
        (when (and location (< (point) (org-element-property :begin headline)))
          (setq next headline)))

       (if next
           (progn
             (org-noter--doc-goto-location
              (org-noter--parse-location-property next))
             (org-noter--focus-notes-region
              (org-noter--make-view-info-for-single-note session next)))
         (user-error "There is no next note")))))

  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions
              #'org-noter-pdftools-jump-to-note)))

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-pomodoro
  :custom
  (org-pomodoro-manual-break t)

  (org-pomodoro-start-sound-p t)
  (org-pomodoro-start-sound (concat user-emacs-directory
                                    "resources/sounds/ripples.wav"))
  (org-pomodoro-finished-sound (concat user-emacs-directory
                                       "resources/sounds/ripples.wav"))
  (org-pomodoro-overtime-sound (concat user-emacs-directory
                                       "resources/sounds/ripples.wav"))
  (org-pomodoro-short-break-sound (concat user-emacs-directory
                                          "resources/sounds/ripples.wav"))
  (org-pomodoro-long-break-sound (concat user-emacs-directory
                                         "resources/sounds/ripples.wav"))
  (org-pomodoro-killed-sound-p t)
  (org-pomodoro-killed-sound (concat user-emacs-directory
                                     "resources/sounds/clock.wav")))

(use-package org-ref
  :after org
  :custom
  (org-ref-default-citation-link "citep")
  :config
  (setq
   org-ref-completion-library 'org-ref-ivy-cite
   org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
   org-ref-default-bibliography (list "~/Sync/Zotero/library.bib")
   org-ref-bibliography-notes "~/org/roam/ref/bib_notes.org"
   org-ref-note-title-format "* TODO %y - %t\n:PROPERTIES:\n:custom_id: %k
:noter_document: %F\n:roam_key: cite:%k\n:author: %9a\n:journal: %j
:year: %y\n:volume: %v\n:pages: %p\n:doi: %D\n:url: %U\n:END:\n\n"
   org-ref-notes-directory "~/org/roam/ref/"
   org-ref-notes-function 'orb-edit-notes))

(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/org/roam")
  (org-roam-buffer-position 'bottom)
  (org-roam-capture-templates
   '(("d" "default" plain #'org-roam-capture--get-point "%?"
      :file-name "notes/%<%Y%m%d%H%M%S>-${slug}"
      :head "#+title: ${title}\n#+setupfile: ~/org/roam/template.org
#+created: %U
#+last_modified: %U\n\n"
      :unnarrowed t)
     ("p" "project" plain #'org-roam-capture--get-point "%?"
      :file-name "projects/%<%Y%m%d%H%M%S>-${slug}"
      :head "#+title: ${title}\n#+setupfile: ~/org/roam/template.org
#+created: %U
#+last_modified: %U\n\n"
      :unnarrowed t)))
  :bind (:map org-roam-mode-map
         (("C-c n l" . org-roam)
          ("C-c n f" . org-roam-find-file)
          ("C-c n g" . org-roam-graph))
         :map org-mode-map
         (("C-c n i" . org-roam-insert))
         (("C-c n I" . org-roam-insert-immediate)))
  :init
  (add-hook 'after-init-hook
            (lambda ()
              (setq time-stamp-start "last_modified:[ ]+\\\\?")
              (setq time-stamp-end "$")
              (setq time-stamp-format "\[%Y-%02m-%02d %3a %02H:%02M\]")
              (add-hook 'before-save-hook #'time-stamp)))
  :config
  (setq org-roam-completion-system 'ivy))

(use-package org-roam-bibtex
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-preformat-keywords
        '("citekey" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "ref/${slug}"
           :head "#+title: ${citekey}: ${title}
#+setupfile: ~/org/roam/template.org
#+roam_key: ${ref}
#+roam_tags: literature
#+created: %U
#+last_modified: %U

* ${title}\n:PROPERTIES:\n:custom_id: ${citekey}\n:url: ${url}
:author: ${author-or-editor}
:noter_document: %(orb-process-file-field \"${citekey}\")\n:noter_page:
:END:\n%?"
           :unnarrowed t))))

(use-package org-super-agenda
  :disabled t
  :after org
  :custom
  (org-super-agenda-groups
   '(;; Each group has an implicit boolean OR operator between its selectors.
     (:name "Today"  ; Optionally specify section name
      :time-grid t  ; Items that appear on the time grid
      :todo "TODAY")  ; Items that have this TODO keyword
     (:name "Important"
      ;; Single arguments given alone
      :tag "bills"
      :priority "A")
     ;; Set order of multiple groups at once
     (:order-multi (2 (:name "Shopping in town"
                       ;; Boolean AND group matches items that match all subgroups
                       :and (:tag "shopping" :tag "@town"))
                      (:name "Food-related"
                       ;; Multiple args given in list with implicit OR
                       :tag ("food" "dinner"))
                      (:name "Personal"
                       :habit t
                       :tag "personal")
                      (:name "Space-related (non-moon-or-planet-related)"
                       ;; Regexps match case-insensitively on the entire entry
                       :and (:regexp ("space" "NASA")
                             ;; Boolean NOT also has implicit OR between selectors
                             :not (:regexp "moon" :tag "planet")))))
     ;; Groups supply their own section names when none are given
     (:todo "WAITING" :order 8)  ; Set order of this section
     (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
      ;; Show this group at the end of the agenda (since it has the
      ;; highest number). If you specified this group last, items
      ;; with these todo keywords that e.g. have priority A would be
      ;; displayed in that group instead, because items are grouped
      ;; out in the order the groups are listed.
      :order 9)
     (:priority<= "B"
      ;; Show this section after "Today" and "Important", because
      ;; their order is unspecified, defaulting to 0. Sections
      ;; are displayed lowest-number-first.
      :order 1)
     ;; After the last group, the agenda will display items that didn't
     ;; match any of these groups, with the default order position of 99
     ))
  :config
  (org-super-agenda-mode))

(use-package ox-hugo
  :after ox
  :init
  (with-eval-after-load 'org-capture
    (defun org-hugo-new-subtree-post-capture-template ()
      "Return `org-capture' template string for new Hugo post."
      (let* ((title (read-from-minibuffer "Post title: "))
             (filename (org-hugo-slug title)))
        (mapconcat #'identity
                   `(,(concat "* TODO " title)
                     ":PROPERTIES:"
                     ,(concat ":export_file_name: " filename)
                     ":END:"
                     "%?TODO: summary"
                     "#+hugo: more"
                     "TODO: content")
                   "\n")))

    (add-to-list 'org-capture-templates
                 '("h"
                   "Hugo post"
                   entry
                   (file+olp "blog.org" "Weblog ideas")
                   (function org-hugo-new-subtree-post-capture-template)))))

(jco/define-bindings global-map
                     '(("C-c a"   . org-agenda)
                       ("C-c c"   . org-capture)
                       ("C-c l"   . org-store-link)
                       ("C-c M-w" . org-copy)
                       ("C-c C-w" . org-refile)))

(use-package ox-slack
  :defer t
  :init
  (evil-leader/set-key-for-mode 'org-mode "z s"
    'org-slack-export-to-clipboard-as-slack))

(defun jco/find-org-file (filename &optional dir)
  "Open file FILENAME in the directory DIR (default: `org-directory')."
  (find-file (concat (or dir org-directory) "/" filename))
  (jco/ensure-todo-org-header))

(provide 'init-org)

;;; init-org.el ends here
