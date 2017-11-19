;;; #init-elfeed.el --- Configuration for elfeed -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(defun jco/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening."
  (interactive)
  (require 'elfeed)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force)
  (elfeed-update))

(defun jco/elfeed-save-db-and-quit ()
  "Wrapper to save the elfeed db to disk before quitting."
  (interactive)
  (elfeed-db-save)
  (quit-window t))

(defun jco/elfeed-db-update ()
  "Update the elfeed db."
  (interactive)
  (elfeed-db-save)
  (elfeed-db-load)
  (elfeed-search-update--force)
  (elfeed-update))

(use-package elfeed
  :defer t
  :bind (:map elfeed-search-mode-map
              ("q" . jco/elfeed-save-db-and-quit))
  :config
  (setq shr-use-fonts nil))

(add-hook 'elfeed-search-mode-hook
          (lambda ()
            (turn-off-fci-mode)
            (jco/define-bindings elfeed-search-mode-map
                                 '(("j" . next-line)
                                   ("k" . previous-line)))))

(defadvice elfeed-show-entry
    (after elfeed-show-refresh-after activate compile)
  "Make text of message be correctly formatted in visual-fill-column-mode."
  (nlinum-mode -1)
  (visual-line-mode)
  (visual-fill-column-mode)
  (turn-off-fci-mode)
  (elfeed-show-refresh))

(use-package elfeed-org
  :after elfeed
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files '("~/.elfeed/elfeed.org")))

(use-package elfeed-web
  :if (and (jco/at-digitalocean-p) (daemonp))
  :config
  (setq http-port 8080)
  (elfeed-web-start)
  (run-with-timer 0 (* 1 60) 'jco/elfeed-db-update))

(provide 'init-elfeed)

;;; init-elfeed.el ends here
