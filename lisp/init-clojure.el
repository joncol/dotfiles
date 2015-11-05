(add-hook 'clojure-mode-hook
          '(lambda ()
             (paredit-mode)
             (evil-paredit-mode)))

(provide 'init-clojure)
