;;; #init-yas.el --- Yasnippet config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package yasnippet
  :config
  (yas-global-mode)
  (setq yas-snippet-dirs (list (concat user-emacs-directory "snippets")))
  (setq yas-indent-line 'auto)
  (setq yas-also-auto-indent-first-line t)
  (yas-reload-all) ;; Needed to unload snippets in elpa dir.
  (add-hook 'snippet-mode-hook
            (lambda ()
              (modify-syntax-entry ?- "w")
              (ethan-wspace-mode -1)))
  (evil-leader/set-key "TAB" 'yas-insert-snippet))

(use-package helm-c-yasnippet
  :bind (([C-tab] . helm-yas-complete))
  :config
  (setq helm-yas-display-key-on-candidate t))

(provide 'init-yas)

;;; init-yas.el ends here
