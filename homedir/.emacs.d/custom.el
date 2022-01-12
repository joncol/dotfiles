(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((haskell-stylish-on-save)
     (haskell-mode-stylish-haskell-path . "brittany")
     (haskell-stylish-on-save . t)
     (eval turn-off-auto-fill)
     (eval visual-fill-column-mode t)
     (org-export-initial-scope . subtree)
     (eval add-hook 'after-save-hook
           (lambda nil
             (load-file user-init-file)
             (org-babel-tangle))
           nil t)
     (cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend")
     (org-archive-location . "::* Archived Tasks")))
 '(warning-suppress-types '((direnv))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :foundry "CTDB" :slant normal :weight medium :height 110 :width normal))))
 '(italic ((t (:underline t)))))
