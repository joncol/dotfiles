;;; #init-qml.el --- QML config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))

(add-hook 'qml-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (jco/cmake-compile-command))
            (jco/define-bindings qml-mode-map '(("<f6>" . compile)))
            (setq comment-start "//"
                  comment-end "")))

(provide 'init-qml)

;;; init-qml.el ends here
