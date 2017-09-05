;;; #init-ruby.el --- Ruby config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package flymake-ruby)

(use-package inf-ruby
  :diminish inf-ruby-mode)

(use-package rbenv
  :config
  (setq rbenv-show-active-ruby-in-modeline nil)
  (global-rbenv-mode))

(use-package robe
  :diminish robe-mode)

(use-package rubocop
  :diminish rubocop-mode)

(use-package ruby-end
  :diminish ruby-end-mode)

(use-package yard-mode
  :diminish yard-mode)

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

(dolist (mode '(hs-minor-mode
                my-flymake-minor-mode
                prettify-symbols-mode
                rubocop-mode
                robe-mode
                ruby-end-mode
                yard-mode))
  (add-hook 'ruby-mode-hook mode))

(add-hook 'ruby-mode-hook
          (lambda ()
            (unless (display-graphic-p)
              (show-paren-mode -1))

            (jco/common-prog)

            (smartparens-mode)

            (eval-after-load "evil" '(setq evil-shift-width 2))

            (add-to-list 'company-backends 'company-robe)
            (flymake-ruby-load)
            (setq prettify-symbols-alist
                  '(("lambda" . ?λ)
                    ("->" . ?λ)))
            (eval-after-load "hideshow"
              '(add-to-list 'hs-special-modes-alist
                            `(ruby-mode
                              ,(rx (or "def" "class" "module" "do" "{" "["))
                              ,(rx (or "}" "]" "end"))
                              ,(rx (or "#" "=begin"))
                              ruby-forward-sexp nil))))

          (eval-after-load "evil"
            '(evil-define-motion evil-ruby-jump-item (count)
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
                      (evil-jump-item count))))))

(provide 'init-ruby)

;;; init-ruby.el ends here
