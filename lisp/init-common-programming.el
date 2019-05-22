;;; Common setup for various programming modes

(dolist (hook '(c-mode-common-hook
                clojure-mode-hook
                conf-unix-mode-hook
                cmake-mode-hook
                csharp-mode-hook
                csv-mode-hook
                cypher-mode-hook
                dockerfile-mode-hook
                emacs-lisp-mode-hook
                fsharp-mode-hook
                graphviz-dot-mode-hook
                haskell-mode-hook
                haskell-cabal-mode-hook
                TeX-mode-hook
                lisp-mode-hook
                lua-mode-hook
                markdown-mode-hook
                nxml-mode-hook
                org-mode-hook
                perl-mode-hook
                python-mode-hook
                qmake-mode-hook
                qml-mode-hook
                racket-mode-hook
                rst-mode-hook
                ruby-mode-hook
                rust-mode-hook
                scheme-mode-hook
                scss-mode-hook
                sh-mode-hook
                sml-mode-hook
                sql-mode-hook
                terraform-mode-hook
                tex-mode-hook
                toml-mode-hook
                yaml-mode-hook))
  (add-hook hook #'jco/common-prog))

(provide 'init-common-programming)
