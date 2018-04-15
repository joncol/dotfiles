;;; #init-ruby.el --- Ruby config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package flymake-ruby
  :defer t)

(use-package inf-ruby
  :defer t)

(use-package rbenv
  :config
  (setq rbenv-show-active-ruby-in-modeline nil)
  (global-rbenv-mode))

(use-package robe
  :defer t
  :bind (:map robe-mode-map
              ("C-c C-k" . ruby-send-buffer)))

(use-package rubocop
  :defer t)

(use-package ruby-end
  :defer t)

(use-package yard-mode
  :defer t)

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

(add-hook 'ruby-mode-hook
          #'(lambda ()
              (hs-minor-mode)
              (my-flymake-minor-mode)
              (prettify-symbols-mode)
              (rubocop-mode)
              (robe-mode)
              (ruby-end-mode)
              (yard-mode)
              (unless (display-graphic-p)
                (show-paren-mode -1))
              (jco/common-prog)
              (smartparens-mode)
              (setq evil-shift-width 2)
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
                                ruby-forward-sexp nil)))
              (with-eval-after-load 'evil
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
                         (evil-jump-item count)))))))

(provide 'init-ruby)

;;; init-ruby.el ends here
