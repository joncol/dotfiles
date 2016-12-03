;;; #init-neotree.el --- Neotree config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package neotree
  :init
  (global-set-key (kbd "<f2>") 'neotree-toggle)

  :bind (:map evil-normal-state-local-map
              ([tab] . neotree-enter)
              ([spc] . neotree-enter)
              ([ret] . neotree-enter)
              ("c" . neotree-change-root)
              ("g" . neotree-refresh)
              ("q" . neotree-hide)
              ("v" . neotree-enter-vertical-split)
              ("s" . neotree-enter-horizontal-split))

  :config
  (setq neo-show-header nil))

(provide 'init-neotree)

;;; init-neotree.el ends here
