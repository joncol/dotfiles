(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

(unless (package-installed-p 'yasnippet)
  (package-refresh-contents))

(let ((packages
       '(ack-and-a-half
         afternoon-theme
         ample-theme
         angular-snippets
         airline-themes
         ag
         auto-complete
         better-defaults
         cider
         cloc
         clojure-mode
         clojure-snippets
         cmake-mode
         company
         company-cabal
         company-ghc
         confluence
         csharp-mode
         csv-mode
         cyberpunk-theme
         dash
         dirtree
         ecb
         edit-server
         epl
         espresso-theme
         ethan-wspace
         evil
         evil-jumper
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
         htmlize
         java-snippets
         jira
         leuven-theme
         lua-mode
         magit
         markdown-mode
         molokai-theme
         monky
         monokai-theme
         neotree
         nyan-mode
         omnisharp
         org-plus-contrib
         org-present
         ox-reveal
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
         robe
         rspec-mode
         rubocop
         ruby-end
         rust-mode
         slime
         sml-mode
         solarized-theme
         soothe-theme
         srefactor
         toml-mode
         undo-tree
         xml-rpc
         yaml-mode
         yard-mode
         yasnippet)))
  (dolist (p packages)
    (unless (package-installed-p p)
      (package-install p))))

(provide 'init-packages)
