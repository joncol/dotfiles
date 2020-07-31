;;; #init-rust.el --- rust configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package racer
  :hook (rust-mode . racer-mode))

(use-package rust-mode
  :hook (rust-mode . lsp)
  :config
  (evil-leader/set-key "x m" #'lsp-ui-imenu))

(provide 'init-rust)

;;; init-rust.el ends here
