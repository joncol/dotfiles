;;; #init-python.el --- Python config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(with-eval-after-load 'python
  (setq python-shell-prompt-detect-failure-warning nil))

(use-package py-autopep8
  :defer t)

(use-package elpy
  :defer t
  ;; :init
  ;; (elpy-enable)
  :config
  (when (require 'flycheck nil t)
    (remove-hook 'elpy-modules 'elpy-module-flymake)
    (remove-hook 'elpy-modules 'elpy-module-yasnippet)
    (remove-hook 'elpy-mode-hook 'elpy-module-highlight-indentation))
  (setq elpy-rpc-backend "jedi"))

(use-package jedi-core
  :defer t
  :bind (:map jedi-mode-map
         ("M-." . jedi:goto-definition)
         ("M-," . jedi:goto-definition-pop-marker)))

(use-package ein
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
            (py-autopep8-enable-on-save)
            (evil-leader/set-key "ap" 'py-autopep8-buffer)
            (setq python-shell-interpreter "jupyter"
                  python-shell-interpreter-args "console --simple-prompt"
                  python-shell-prompt-detect-failure-warning nil)
            (add-to-list 'python-shell-completion-native-disabled-interpreters
                         "jupyter")
            (when (not (eq system-type 'windows-nt))
              (require 'jedi-core)
              (jedi-mode)
              (require 'elpy)
              (elpy-mode))))

(provide 'init-python)

;;; init-python.el ends here
