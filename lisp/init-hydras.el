(global-unset-key (kbd "<f1>"))

(global-set-key (kbd "<f1>") 'jco/hydra-main-menu/body)

(defhydra jco/hydra-main-menu ()
  "common ops"
  ("e" (lambda ()
         (interactive)
         (find-file (concat user-emacs-directory "init.el")))
   "edit configuration file")
  ("m" (mu4e) "mail"))

(provide 'init-hydras)
