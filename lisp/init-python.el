;;; #init-python.el --- Python config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(with-eval-after-load 'python
  (setq python-shell-prompt-detect-failure-warning nil))

(use-package elpy
  :if (not (eq system-type 'windows-nt))
  :diminish elpy-mode
  :diminish highlight-indentation-mode

  :init
  (when (require 'flycheck nil t)
    (remove-hook 'elpy-modules 'elpy-module-flymake)
    (remove-hook 'elpy-modules 'elpy-module-yasnippet)
    (remove-hook 'elpy-mode-hook 'elpy-module-highlight-indentation))

  (elpy-enable)

  :config
  (setq elpy-rpc-backend "jedi"))

(use-package jedi-core
  :diminish jedi-mode)

(use-package company-jedi
  :if (not (eq system-type 'windows-nt))
  :init
  (add-hook 'python-mode-hook #'jedi-mode)
  (with-eval-after-load 'python
    (add-to-list 'company-backends '(company-jedi company-files))))

(provide 'init-python)

;;; init-python.el ends here
