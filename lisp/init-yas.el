(setq yas-snippet-dirs (list (concat user-emacs-directory "snippets")))
(yas-global-mode)
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

(defun tab-indent-or-complete (use-ghc-complete)
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (progn
              (company-complete-common)
              (when use-ghc-complete
                (ghc-complete)))
          (indent-for-tab-command)))))

(global-set-key [tab] (lambda () (interactive) (tab-indent-or-complete nil)))

(provide 'init-yas)
