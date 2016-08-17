(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

(unless (package-installed-p 'yasnippet)
  (package-refresh-contents))

(let ((packages
       '(ace-jump-mode
         ack-and-a-half
         afternoon-theme
         ample-theme
         angular-snippets
         airline-themes
         ample-zen-theme
         assemblage-theme
         ag
         auto-complete
         better-defaults
         borland-blue-theme
         cherry-blossom-theme
         cider
         cloc
         clojure-mode
         clojure-snippets
         cmake-mode
         command-log-mode
         company
         company-cabal
         company-ghc
         confluence
         csharp-mode
         csv-mode
         cyberpunk-theme
         darktooth-theme
         dash
         dirtree
         ecb
         edit-server
         epl
         espresso-theme
         ethan-wspace
         evil
         evil-leader
         evil-numbers
         evil-matchit
         evil-nerd-commenter
         evil-paredit
         evil-search-highlight-persist
         evil-surround
         exec-path-from-shell
         fill-column-indicator
         flatland-theme
         flatui-theme
         flx-ido
         flycheck
         flymake-ruby
         fsharp-mode
         ggtags
         ghci-completion
         glsl-mode
         go-snippets
         gotham-theme
         goto-chg
         goto-last-change
         grandshell-theme
         graphviz-dot-mode
         gruber-darker-theme
         gruvbox-theme
         guide-key
         haskell-mode
         helm
         helm-ag
         helm-company
         helm-gtags
         helm-projectile
         helm-swoop
         hemisu-theme
         highlight2clipboard
         htmlize
         irfc
         java-snippets
         jira
         json-mode
         leuven-theme
         lua-mode
         magit
         markdown-mode
         molokai-theme
         monky
         monokai-theme
         neotree
         nsis-mode
         nyan-mode
         omnisharp
         org-plus-contrib
         org-present
         organic-green-theme
         ox-reveal
         pandoc-mode
         paredit
         pkg-info
         plantuml-mode
         popup
         pos-tip
         powerline
         project-explorer
         projectile
         qml-mode
         racket-mode
         rvm
         rainbow-delimiters
         rainbow-mode
         redshank
         rnc-mode
         robe
         rspec-mode
         rubocop
         ruby-end
         rust-mode
         slime
         sml-mode
         smtpmail-multi
         solarized-theme
         soothe-theme
         string-inflection
         toml-mode
         undo-tree
         volatile-highlights
         xml-rpc
         yaml-mode
         yard-mode
         yasnippet)))
  (dolist (p packages)
    (unless (package-installed-p p)
      (package-install p))))

(provide 'init-packages)
