;;; #init-python.el --- Python config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(with-eval-after-load 'python
  (setq python-shell-prompt-detect-failure-warning nil))

(use-package elpy
  :defer t
  ;; :init
  ;; (elpy-enable)
  :config
  (when (require 'flycheck nil t)
    (remove-hook 'elpy-modules 'elpy-module-flymake)
    (remove-hook 'elpy-modules 'elpy-module-yasnippet)
    (remove-hook 'elpy-mode-hook 'elpy-module-highlight-indentation))
  :config
  (setq elpy-rpc-backend "jedi"))

(use-package jedi-core
  :defer t)

(use-package company-jedi
  :if (not (eq system-type 'windows-nt))
  :defer t
  :config
  (with-eval-after-load 'python
    (add-to-list 'company-backends '(company-jedi company-files))))

(add-hook 'elpy-mode-hook
          (lambda ()
            (jco/define-bindings elpy-mode-map
                                 '(("C-c C-k" . python-shell-send-buffer)
                                   ("C-M-x" . python-shell-send-defun)))))

(add-hook 'python-mode-hook
          (lambda ()
            (when (not (eq system-type 'windows-nt))
              (require 'jedi-core)
              (jedi-mode)
              (require 'elpy)
              (elpy-mode))))

(provide 'init-python)

;;; init-python.el ends here
