((nil . ((eval . (add-to-list 'lsp-language-id-configuration
                              '(conf-colon-mode . "json")))
         (project-vc-ignores . ("scrivepdftools/"
                                "texts/"
                                "vagrant/"))
         (lsp-file-watch-threshold . 20000)
         (add-to-list 'auto-mode-alist
                      '("\\.conf.template\\'" . conf-colon-mode))))

 (conf-colon-mode . ((eval . (lsp-deferred))))

 (haskell-mode . ((eval (push '(cabal-fmt . ("cabal-fmt" "--indent 4"))
                              apheleia-formatters))
                  (apheleia-formatter . brittany)
                  (fill-column . 90)))
 ;; (haskell-mode . ((fill-column . 90)))

 (haskell-cabal-mode . ((eval . (apheleia-mode -1))))

 (json-mode . ((eval . (apheleia-mode -1)))))
