(global-unset-key (kbd "<f1>"))

(global-set-key (kbd "<f1>") 'jco/hydra-main-menu/body)

(defun open-config-file (file-name)
  (interactive)
  (find-file (concat user-emacs-directory file-name)))

(defhydra jco/hydra-main-menu (:color blue)
  "Menu"
  ("e" jco/hydra-edit-config/body "edit config")
  ("m" mu4e "mail")
  ("h" monky-status "monky status")
  ("a" jco/hydra-apropos/body "apropos")
  )

(defhydra jco/hydra-edit-config (:color blue)
  "Edit config"
  ("c" (open-config-file "lisp/init-common.el") "common")
  ("t" (open-config-file "lisp/init-theme.el") "theme")
  )

(defhydra jco/hydra-apropos (:color blue :hint nil)
  "Apropos"
  ("a" apropos "apropos")
  ("c" apropos-command "cmd")
  ("d" apropos-documentation "doc")
  ("e" apropos-value "val")
  ("l" apropos-library "lib")
  ("o" apropos-user-option "option")
  ("v" apropos-variable "var")
  ("i" info-apropos "info")
  ("t" tags-apropos "tags"))

(provide 'init-hydras)
