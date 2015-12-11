(require 'nxml-mode)

(define-key nxml-mode-map (kbd "C-c C-p") 'rng-previous-error)

(add-to-list 'auto-mode-alist '("\\.rnc\\'" . rnc-mode))

(provide 'init-nxml)
