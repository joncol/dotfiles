;;; #init-modeline.el --- Configuration of modeline -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package rich-minority
  :config
  (setq rm-blacklist ".")
  (rich-minority-mode))

(use-package smart-mode-line
  :disabled t
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-init))

(provide 'init-modeline)

;;; init-modeline.el ends here
