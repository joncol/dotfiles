;;; #init-lua.el --- Lua config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package company-lua
  :defer t
  :commands lua-mode)

(use-package lua-mode
  :defer t
  :commands lua-mode
  :config
  (setq evil-shift-width 2)
  (setq lua-indent-level 2)
  (add-to-list 'company-backends 'company-lua))

(add-hook 'lua-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")
            (smartparens-mode)))

(provide 'init-lua)

;;; init-lua.el ends here
