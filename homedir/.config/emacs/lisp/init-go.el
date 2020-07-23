;;; #init-go.el --- Go configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package go-mode
  :defer t
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'gofmt-before-save)
              (evil-leader/set-key "h d" 'godoc-at-point)
              (local-set-key (kbd "M-.") 'godef-jump)
              (local-set-key (kbd "M-,") 'pop-tag-mark))))

(provide 'init-go)

;;; init-go.el ends here
