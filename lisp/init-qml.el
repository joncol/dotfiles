(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))

(add-hook 'qml-mode-hook
          (lambda ()
            (setq comment-start "//"
                  comment-end "")))

(provide 'init-qml)
