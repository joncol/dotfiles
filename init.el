;; (package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(defvar jco/init-errors nil
  "If there are any initialization errors, they will be appended to this list.")

(require 'init-bootstrap)
(jco/safe-load-init-files)

(message "==============================")
(message (if jco/init-errors
             (mapconcat #'identity jco/init-errors "\n")
           "Emacs initialized successfully"))
(message "==============================")

(custom-set-variables
 '(ecb-options-version "2.40")
 '(large-file-warning-threshold nil)
 '(package-selected-packages
   (quote
    (doom-themes yard-mode yaml-mode volatile-highlights toml-mode string-inflection soothe-theme solarized-theme smtpmail-multi sml-mode slime rvm rust-mode ruby-end rubocop rspec-mode robe rnc-mode redshank rainbow-mode rainbow-delimiters racket-mode qml-mode project-explorer plantuml-mode pandoc-mode ox-reveal organic-green-theme org-present org-plus-contrib omnisharp nyan-mode nsis-mode neotree monokai-theme monky molokai-theme material-theme markdown-mode magit lua-mode leuven-theme json-mode jira java-snippets irfc hindent highlight2clipboard hemisu-theme helm-swoop helm-projectile helm-gtags helm-company helm-ag haskell-snippets guide-key gruvbox-theme gruber-darker-theme graphviz-dot-mode grandshell-theme goto-last-change gotham-theme go-snippets gnuplot-mode gnuplot glsl-mode ghci-completion ggtags fsharp-mode flymake-ruby flx-ido flatui-theme flatland-theme fill-column-indicator exec-path-from-shell evil-surround evil-search-highlight-persist evil-paredit evil-numbers evil-nerd-commenter evil-matchit evil-leader ethan-wspace espresso-theme edit-server ecb ebal dirtree darktooth-theme cyberpunk-theme csv-mode confluence company-ghc company-cabal command-log-mode cmake-mode clojure-snippets cloc cider cherry-blossom-theme borland-blue-theme better-defaults avy assemblage-theme angular-snippets ample-zen-theme ample-theme all-the-icons airline-themes ag afternoon-theme ack-and-a-half ace-jump-mode)))
 '(safe-local-variable-values (quote ((org-archive-location . "::* Archived Tasks"))))
 '(send-mail-function (quote smtpmail-send-it)))

(put 'narrow-to-region 'disabled nil)
