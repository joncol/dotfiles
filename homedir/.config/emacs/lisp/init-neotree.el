;;; #init-neotree.el --- Neotree config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package neotree
  :init
  (global-set-key (kbd "<f2>") 'neotree-toggle)

  :config
  (setq neo-show-header nil))

(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map
              (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map
              (kbd "SPC") 'neotree-enter)
            (define-key evil-normal-state-local-map
              (kbd "RET") 'neotree-enter)
            (define-key evil-normal-state-local-map
              (kbd "c") 'neotree-change-root)
            (define-key evil-normal-state-local-map
              (kbd "g") 'neotree-refresh)
            (define-key evil-normal-state-local-map
              (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map
              (kbd "v") 'neotree-enter-vertical-split)
            (define-key evil-normal-state-local-map
              (kbd "s") 'neotree-enter-horizontal-split)))

(provide 'init-neotree)

;;; init-neotree.el ends here
