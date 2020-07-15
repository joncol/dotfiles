;;; #init-rust.el --- rust configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package flycheck-rust
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package racer
  :config
  (add-hook 'rust-mode-hook #'racer-mode))

(use-package rust-mode
  :defer t
  :bind (:map rust-mode-map
         ("C-c C-c" . rust-run)))

(provide 'init-rust)

;;; init-rust.el ends here
