(setq org-src-fontify-natively t)

(add-hook 'org-mode-hook
          (lambda ()
            (load-library "ox-reveal")
            (auto-fill-mode)
            (global-unset-key (kbd "C-x C-v"))

             ;;; Embedding youtube links in org-mode
             ;;; TODO: extract to function

            (let ((yt-iframe-format
                   (concat "<iframe width=\"560\""
                           " height=\"315\""
                           " src=\"https://www.youtube.com/embed/%s?rel=0\""
                           " frameborder=\"0\""
                           " allowfullscreen>%s</iframe>"))
                  (ytnc-iframe-format
                   (concat "<iframe width=\"560\""
                           " height=\"315\""
                           " src=\"https://www.youtube.com/embed/%s?rel=0&controls=0\""
                           " frameborder=\"0\""
                           " allowfullscreen>%s</iframe>")))


              (org-add-link-type
               "yt"
               (lambda (handle)
                 (browse-url
                  (concat "https://www.youtube.com/embed/"
                          handle)))
               (lambda (path desc backend)
                 (cl-case backend
                   (html (format yt-iframe-format
                                 path (or desc "")))
                   (latex (format "\href{%s}{%s}"
                                  path (or desc "video"))))))

              (org-add-link-type
               "ytnc"
               (lambda (handle)
                 (browse-url
                  (concat "https://www.youtube.com/embed/"
                          handle)))
               (lambda (path desc backend)
                 (cl-case backend
                   (html (format ytnc-iframe-format
                                 path (or desc "")))
                   (latex (format "\href{%s}{%s}"
                                  path (or desc "video")))))))))

(provide 'init-org)
