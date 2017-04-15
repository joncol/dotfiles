;;; #init-ruby.el --- Ruby config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package inf-ruby)
(use-package robe)
(use-package rubocop)
(use-package ruby-end)

(use-package rvm
  :init
  (setq rvm-executable "~/.rvm/bin/rvm"))

(use-package yard-mode)

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

(defvar jco/rvm-use-default-called nil)

(defadvice rvm-use-default (after rvm-use-default-after activate compile)
  (setq jco/rvm-use-default-called t))

(rvm-use-default)

(add-hook 'ruby-mode-hook
          (lambda ()
            (unless (display-graphic-p)
              (show-paren-mode -1))

            (jco/common-prog)

            (eval-after-load "evil" '(setq evil-shift-width 2))

            (unless jco/rvm-use-default-called
              (rvm-use-default))

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
                      (evil-jump-item count)))))

          (bind-keys :map ruby-mode-map ("C-c r a" . rvm-activate-corresponding-ruby)))

(provide 'init-ruby)

;;; init-ruby.el ends here
