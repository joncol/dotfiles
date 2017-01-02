;;; #init-qml.el --- QML config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))

(use-package qml-mode
  :bind (:map qml-mode-map
              ([f6] . compile))
  :config
  (add-hook 'qml-mode-hook
            (lambda ()
              (set (make-local-variable 'compile-command)
                   (jco/cmake-compile-command))))
  (setq comment-start "//"
        comment-end ""))

(provide 'init-qml)

;;; init-qml.el ends here
