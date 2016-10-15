;;; Common setup for various programming modes

(dolist (hook '(c-mode-common-hook
                clojure-mode-hook
                conf-unix-mode-hook
                cmake-mode-hook
                csharp-mode-hook
                emacs-lisp-mode-hook
                fsharp-mode-hook
                graphviz-dot-mode-hook
                haskell-mode-hook
                haskell-cabal-mode-hook
                latex-mode-hook
                lisp-mode-hook
                lua-mode-hook
                markdown-mode-hook
                nxml-mode-hook
                org-mode-hook
                python-mode-hook
                qml-mode-hook
                racket-mode-hook
                ruby-mode-hook
                rust-mode-hook
                scheme-mode-hook
                sml-mode-hook
                tex-mode-hook
                toml-mode-hook
                yaml-mode-hook
                ))
  (add-hook hook 'jco/common-prog))

(provide 'init-common-programming)
