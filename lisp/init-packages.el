(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

(setq jco/package-list-refreshed nil)

(defun jco/refresh-package-list ()
  (unless jco/package-list-refreshed
    (package-refresh-contents)
    (setq jco/package-list-refreshed t)))

(let ((packages
       '(ace-flyspell
         ace-jump-helm-line
         ace-jump-mode
         ace-link
         ace-window
         ack-and-a-half
         ag
         anzu
         auto-complete
         avy
         beacon
         better-defaults
         cider
         circe
         cloc
         clojure-mode
         cmake-ide
         cmake-mode
         command-log-mode
         company
         company-cabal
         confluence
         csharp-mode
         csv-mode
         dash
         diminish
         dired+
         dirtree
         ebal
         ecb
         edit-server
         epl
         erc-hl-nicks
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
         expand-region
         fancy-narrow
         fill-column-indicator
         flx-ido
         flycheck
         flycheck-pos-tip
         flymake-ruby
         fsharp-mode
         fuzzy
         ggtags
         ghci-completion
         glsl-mode
         gnuplot
         gnuplot-mode
         golden-ratio
         google-this
         goto-chg
         goto-last-change
         graphviz-dot-mode
         guide-key
         haskell-mode
         helm
         helm-ag
         helm-company
         helm-descbinds
         helm-gtags
         helm-projectile
         helm-swoop
         highlight2clipboard
         hindent
         htmlize
         iedit
         impatient-mode
         info+
         intero
         irfc
         jira
         json-mode
         langtool
         lua-mode
         magit
         markdown-mode
         monky
         neotree
         nsis-mode
         nyan-mode
         omnisharp
         org-plus-contrib
         org-pomodoro
         org-present
         ox-reveal
         package-utils
         pandoc-mode
         paradox
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
         rainbow-delimiters
         rainbow-mode
         redshank
         rnc-mode
         robe
         rspec-mode
         rtags
         rubocop
         ruby-end
         rust-mode
         rvm
         slime
         sml-mode
         smtpmail-multi
         spaceline
         string-inflection
         sx
         toml-mode
         undo-tree
         use-package
         volatile-highlights
         xml-rpc
         yaml-mode
         yard-mode
         yasnippet)))
  (dolist (p packages)
    ;; (message (format "Package %s status: %s" p (package-installed-p p)))
    (unless (package-installed-p p)
      (jco/refresh-package-list)
      (package-install p))))

(let ((themes
       '(afternoon-theme
         ample-theme
         airline-themes
         ample-zen-theme
         assemblage-theme
         borland-blue-theme
         cherry-blossom-theme
         color-theme-sanityinc-tomorrow
         cyberpunk-theme
         darktooth-theme
         doom-themes
         espresso-theme
         flatland-theme
         flatui-theme
         gotham-theme
         grandshell-theme
         gruber-darker-theme
         gruvbox-theme
         hemisu-theme
         leuven-theme
         material-theme
         meacupla-theme
         minimal-theme
         molokai-theme
         monokai-theme
         organic-green-theme
         reykjavik-theme
         solarized-theme
         soothe-theme
         tao-theme)))
  (dolist (p themes)
    (unless (package-installed-p p)
      (package-install p))))

(provide 'init-packages)
