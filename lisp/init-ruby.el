(require 'inf-ruby)

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

(dolist (mode '(yard-mode
                ruby-end-mode
                my-flymake-minor-mode
                rubocop-mode
                robe-mode))
  (add-hook 'ruby-mode-hook mode))

(defvar jco/rvm-use-default-called nil)

(defadvice rvm-use-default (after rvm-use-default-after activate compile)
  (setq jco/rvm-use-default-called t))

(add-hook 'ruby-mode-hook
          (lambda ()
             (jco/common-prog)
             (setq evil-shift-width 2)
             (unless jco/rvm-use-default-called
               (rvm-use-default))
             (add-to-list 'company-backends 'company-robe)
             (flymake-ruby-load)
             (setq prettify-symbols-alist
               '(("lambda" . ?λ)
                 ("->" . ?λ)))
             (prettify-symbols-mode)))

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

(jco/define-bindings ruby-mode-map
                     '(("C-c r a"  . rvm-activate-corresponding-ruby)))

(provide 'init-ruby)
