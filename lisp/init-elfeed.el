;;; #init-elfeed.el --- Configuration for elfeed -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package elfeed
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

(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files '("~/Dropbox/emacs/elfeed.org")))
(provide 'init-elfeed)

;;; init-elfeed.el ends here
