;;; #init-lua.el --- Lua config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package company-lua
  :after lua-mode)

(use-package lua-mode
  :defer t
  :config
  (setq evil-shift-width 2)
  (setq lua-indent-level 2)
  (add-hook 'lua-mode-hook
            (lambda ()
              (if (getenv "LUA_PATH")
                  (setenv "LUA_PATH"
                          (concat (getenv "LUA_PATH") ";"
                                  (expand-file-name
                                   "~/.luarocks/share/lua/5.1/\?.lua")))
                (setenv "LUA_PATH"
                        (expand-file-name
                         "~/.luarocks/share/lua/5.1/\?.lua")))
              (setq-local company-backends '((company-lua
                                              company-etags
                                              company-dabbrev-code
                                              company-yasnippet))))))

(provide 'init-lua)

;;; init-lua.el ends here
