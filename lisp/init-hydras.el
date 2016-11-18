(global-unset-key (kbd "<f1>"))

(global-set-key (kbd "<f1>") 'jco/hydra-main-menu/body)

(defhydra jco/hydra-main-menu (:color blue)
  "common ops"
  ("e" (lambda ()
         (interactive)
         (find-file (concat user-emacs-directory "init.el")))
   "edit config")
  ("m" (mu4e) "mail")
  ("h" (monky-status) "monky status"))

(provide 'init-hydras)
