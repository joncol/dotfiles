;;; #init-yas.el --- Yasnippet config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package yasnippet
  :defer t
  :config
  (yas-global-mode)
  (setq yas-snippet-dirs (list (concat user-emacs-directory "snippets")))
  ;; yas-indent-line has to be nil to avoid error when expanding `db' snippet.
  (setq yas-indent-line nil)
  (setq yas-also-auto-indent-first-line t)
  (yas-reload-all) ;; Needed to unload snippets in elpa dir.
  (add-hook 'snippet-mode-hook
            (lambda ()
              (modify-syntax-entry ?- "w")
              (ethan-wspace-mode -1)))
  (evil-leader/set-key "TAB" 'yas-insert-snippet))

(use-package helm-c-yasnippet
  :defer t
  :bind (([C-tab] . helm-yas-complete))
  :config
  (setq helm-yas-display-key-on-candidate t))

(provide 'init-yas)

;;; init-yas.el ends here
