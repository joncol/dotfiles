(require 'company)

(setq yas-snippet-dirs (list (concat user-emacs-directory "snippets")))
(yas-global-mode)
(diminish 'yas-minor-mode)
(setq yas-indent-line 'auto)
(setq yas-also-auto-indent-first-line t)

(add-hook 'snippet-mode-hook
          (lambda ()
            (ethan-wspace-mode -1)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(provide 'init-yas)
