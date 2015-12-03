(require 'smartparens-config)

(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

(defun jco/sp-wrap-with-parens (&optional arg)
  "Wrap sexp at point with parentheses."
  (interactive "P")
  )

(jco/define-bindings smartparens-mode-map
                     '(("M-("      . (lambda ()
                                       (interactive)
                                       (sp-wrap-with-pair "(")))
                       ("M-["      . (lambda ()
                                       (interactive)
                                       (sp-wrap-with-pair "[")))
                       ("M-{"      . (lambda ()
                                       (interactive)
                                       (sp-wrap-with-pair "{")))
                       ("M-s"      . sp-splice-sexp)
                       ("M-<up>"   . sp-splice-sexp-killing-backward)
                       ("M-<down>" . sp-splice-sexp-killing-forward)
                       ("M-r"      . sp-raise-sexp)
                       ("C-("      . sp-backward-slurp-sexp)
                       ("C-)"      . sp-forward-slurp-sexp)
                       ("C-{"      . sp-backward-barf-sexp)
                       ("C-}"      . sp-forward-barf-sexp)
                       ("M-S"      . sp-split-sexp)
                       ("M-J"      . sp-join-sexp)
                       ("C-k"      . sp-kill-sexp)))

(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

(provide 'init-smartparens)
