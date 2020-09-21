;;; #init-rust.el --- rust configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package cargo
  :hook (rust-mode . cargo-minor-mode)
  :bind (:map cargo-process-mode-map
         ("n" . evil-search-next)))

(use-package flycheck-rust
  :defer t
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package racer
  :hook (rust-mode . racer-mode))

(use-package rust-mode
  :hook (rust-mode . lsp)
  :config
  (evil-leader/set-key "x m" #'lsp-ui-imenu)
  (sp-pair "\'" nil :actions :rem)
  (add-hook 'rust-mode-hook
            (lambda ()
              (modify-syntax-entry ?! "w"))))

(provide 'init-rust)

;;; init-rust.el ends here
