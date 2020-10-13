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
  :hook (rust-mode . racer-mode)
  :config
  (add-hook 'racer-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '((company-capf company-files)))
              (setq company-minimum-prefix-length 1))))

(use-package rust-mode
  :defer t
  :config
  (sp-pair "\'" nil :actions :rem)
  (add-hook 'rust-mode-hook
            (lambda ()
              (modify-syntax-entry ?! "w"))))

(provide 'init-rust)

;;; init-rust.el ends here
