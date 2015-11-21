(setq org-src-fontify-natively t)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)
(setq org-reveal-hlevel 2)
(setq org-todo-keyword-faces
      '(("IN_PROGRESS" . "orange")))

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

(add-hook 'org-mode-hook
          (lambda ()
            (load-library "ox-reveal")
            (auto-fill-mode)
            (global-unset-key (kbd "C-x C-v"))

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
