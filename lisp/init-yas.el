;;; #init-yas.el --- Yasnippet config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package yasnippet
  :init
  (yas-global-mode)

  :config
  (setq yas-snippet-dirs (list (concat user-emacs-directory "snippets")))
  (setq yas-indent-line 'auto)
  (setq yas-also-auto-indent-first-line t)

  ;; Needed to unload snippets in elpa dir.
  (yas-reload-all)

  ;; Turn off ethan-wspace-mode when editing snippets.
  (add-hook 'snippet-mode-hook (lambda ()
                                 (ethan-wspace-mode -1)))

  (evil-leader/set-key "TAB" 'yas-insert-snippet)


  :diminish yas-minor-mode)

(use-package helm-c-yasnippet
  :bind (([C-tab] . helm-yas-complete))
  :config
  (setq helm-yas-display-key-on-candidate t))

(provide 'init-yas)

;;; init-yas.el ends here
