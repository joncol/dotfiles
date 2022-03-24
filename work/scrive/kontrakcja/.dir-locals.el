((nil . ((eval . (add-to-list 'lsp-language-id-configuration
                              '(".*\\.conf$" . "json")))
         (project-vc-ignores . ("scrivepdftools/"
                                "texts/"
                                "vagrant/"))
         (lsp-file-watch-threshold . 20000)))

 (conf-colon-mode . ((eval . (lsp-deferred))))

 (haskell-mode . ((apheleia-formatter . brittany)
                  (fill-column . 90)))

 (haskell-cabal-mode . ((eval . (apheleia-mode -1))))

 (json-mode . ((eval . (apheleia-mode -1)))))
