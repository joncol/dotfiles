;;; #init-packages.el --- Install used packages -*- lexical-binding: t; -*-

;;; Commentary:

;; Takes care of installing all used packages from package archives.

;;; Code:

(require 'init-security)

(require 'package)
(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(defvar jco/package-list-refreshed)

(set (make-local-variable 'jco/package-list-refreshed) nil)

(defun jco/refresh-package-list ()
  "Refresh package list if it has not already been done."
  (unless jco/package-list-refreshed
    (message "Refreshing package contents")
    (package-refresh-contents)
    (setq jco/package-list-refreshed t)))

(let ((packages
       '(ace-flyspell
         ace-isearch
         ace-jump-helm-line
         ace-jump-mode
         ace-link
         ace-window
         ag
         anzu
         auto-complete
         avy
         better-defaults
         bookmark+
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
         ;; csv-mode
         darkane-theme
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
         nlinum
         nsis-mode
         nyan-mode
         omnisharp
         org-pomodoro
         org-present
         ox-reveal
         package-utils
         pandoc-mode
         ;; paradox
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
         spinner
         string-inflection
         sx
         toml-mode
         undo-tree
         unkillable-scratch
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
      (message (format "Installing package: %s" p))
      (package-install p))))

(let ((themes
       '(afternoon-theme
         ample-theme
         airline-themes
         ample-zen-theme
         ;; assemblage-theme
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
      (jco/refresh-package-list)
      (package-install p))))

(provide 'init-packages)

;;; init-packages.el ends here
