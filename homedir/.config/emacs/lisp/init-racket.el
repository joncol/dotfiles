(when (not (eq system-type 'windows-nt))
  (setq racket-program "/usr/local/bin/racket")
  (setq raco-program "/usr/local/bin/raco"))

(eval-after-load 'racket-mode
  '(define-key racket-mode-map (kbd "C-c C-l") 'racket-run))

(provide 'init-racket)
