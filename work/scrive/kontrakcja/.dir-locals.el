((nil . ((eval . (progn (add-to-list 'lsp-language-id-configuration
                                     '(conf-colon-mode . "json"))
                        (cl-pushnew '(cabal-fmt . ("cabal-fmt" "--indent" "4"))
                                    apheleia-formatters :test #'equal)
                        (cl-pushnew '(nixfmt . ("alejandra"))
                                    apheleia-formatters :test #'equal)))
         (project-vc-ignores . ("scrivepdftools/"
                                "texts/"
                                "vagrant/"))
         (lsp-file-watch-threshold . 20000)
         (add-to-list 'auto-mode-alist
                      '("\\.conf.template\\'" . conf-colon-mode))))

 (conf-colon-mode . ((eval . (lsp-deferred))))

 (haskell-mode . ((fill-column . 90)))

 (haskell-cabal-mode . ((eval . (direnv-update-environment))))

 (json-mode . ((eval . (apheleia-mode -1)))))
