(dolist (fp '("\\.rb$"
              "\\.ru$"
              "\\.rake"
              "\\.jbuilder$"
              "\\.gemspec$"
              "\\GuardFile$"
              "\\Rakefile$"
              "\\Vagrantfile$"
              "\\Gemfile$"
              "\\Godfile$"
              "\\.god$"))
  (add-to-list 'auto-mode-alist `(,fp . ruby-mode)))

(add-hook 'ruby-mode-hook 'yard-mode)
(add-hook 'ruby-mode-hook 'ruby-end-mode)
(add-hook 'ruby-mode-hook 'my-flymake-minor-mode)
(add-hook 'ruby-mode-hook 'rubocop-mode)

(add-hook 'ruby-mode-hook
          '(lambda ()
             (jco/common-prog)
             (setq evil-shift-width 2)
             (rvm-use-default)
             (add-to-list 'company-backends 'company-robe)
             (flymake-ruby-load)))

(evil-define-motion evil-ruby-jump-item (count)
  :jump t
  :type inclusive
  (cond ((or (string-match ruby-block-beg-re (current-word))
             (string-match "describe" (current-word))
             (string-match "context" (current-word))
             (string-match "it" (current-word)))
         (ruby-end-of-block count))
        ((string-match ruby-block-end-re (current-word))
         (ruby-beginning-of-block count))
        (t
         (evil-jump-item count))))

(setq ruby-align-chained-calls nil
                   ruby-align-to-stmt-keywords nil
                   ruby-deep-indent-paren nil
                   ruby-deep-indent-paren-style nil
                   ruby-use-smie nil)

(jco/define-bindings ruby-mode-map
                     '(("C-c r a"  . rvm-activate-corresponding-ruby)))

(provide 'init-ruby)
