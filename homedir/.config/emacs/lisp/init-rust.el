;;; #init-rust.el --- rust configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package rustic
  :defer t
  :config
  (add-hook 'rustic-mode-hook
            (lambda ()
              (sp-pair "\'" nil :actions :rem)
              (modify-syntax-entry ?! "w"))))

(provide 'init-rust)

;;; init-rust.el ends here
