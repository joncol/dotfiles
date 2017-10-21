;;; #init-elfeed.el --- Configuration for elfeed -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package elfeed
  :bind (("U" . elfeed-update))
  :config
  (add-hook 'elfeed-search-mode-hook
            (lambda ()
              (turn-off-fci-mode)))
  (add-hook 'elfeed-show-mode-hook
            (lambda ()
              (turn-off-fci-mode)
              (visual-line-mode)
              (visual-fill-column-mode)))
  (setq shr-use-fonts nil))

(use-package elfeed-goodies
  :config
  (elfeed-goodies/setup))

(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files '("~/Dropbox/emacs/elfeed.org")))

(defun elfeed-goodies/search-header-draw ()
  "Return the string to be used as the Elfeed header."
  (if (zerop (elfeed-db-last-update))
      (elfeed-search--intro-header)
    (let* ((separator-left (intern
                            (format "powerline-%s-%s"
                                    elfeed-goodies/powerline-default-separator
                                    (car powerline-default-separator-dir))))
           (separator-right (intern
                             (format "powerline-%s-%s"
                                     elfeed-goodies/powerline-default-separator
                                     (cdr powerline-default-separator-dir))))
           (db-time (seconds-to-time (elfeed-db-last-update)))
           (stats (-elfeed/feed-stats))
           (search-filter (cond
                           (elfeed-search-filter-active
                            "")
                           (elfeed-search-filter
                            elfeed-search-filter)
                           (""))))
      (if (>= (window-width) (* (frame-width) elfeed-goodies/wide-threshold))
          (search-header/draw-wide separator-left
                                   separator-right
                                   search-filter
                                   stats db-time)
        (search-header/draw-tight separator-left
                                  separator-right
                                  search-filter
                                  stats db-time)))))

(defun elfeed-goodies/entry-line-draw (entry)
  "Print ENTRY to the buffer."
  (let* ((title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
         (date (elfeed-search-format-date (elfeed-entry-date entry)))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title
          (when feed
            (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str (concat "[" (mapconcat 'identity tags ",") "]"))
         (title-width (- (window-width) elfeed-goodies/feed-source-column-width
                         elfeed-goodies/tag-column-width 4))
         (tag-column (elfeed-format-column
                      tags-str (elfeed-clamp (length tags-str)
                                             elfeed-goodies/tag-column-width
                                             elfeed-goodies/tag-column-width)
                      :left))
         (feed-column (elfeed-format-column
                       feed-title
                       (elfeed-clamp elfeed-goodies/feed-source-column-width
                                     elfeed-goodies/feed-source-column-width
                                     elfeed-goodies/feed-source-column-width)
                       :left)))

    (if (>= (window-width) (* (frame-width) elfeed-goodies/wide-threshold))
        (progn
          (insert (propertize date 'face 'elfeed-search-date-face) " ")
          (insert (propertize feed-column 'face 'elfeed-search-feed-face) " ")
          (insert (propertize tag-column 'face 'elfeed-search-tag-face) " ")
          (insert (propertize title 'face title-faces 'kbd-help title)))
      (insert (propertize title 'face title-faces 'kbd-help title)))))

(provide 'init-elfeed)

;;; init-elfeed.el ends here
