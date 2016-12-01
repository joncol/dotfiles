;;; #init-theme.el --- Theme settings -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

;;; Change this to whatever theme you want.
(defvar jco/theme)
(set (make-local-variable 'jco/theme) 'darkane)

(require 'powerline)
(require 'spaceline-config)

(use-package spaceline
  :ensure t
  :init
  (setq powerline-default-separator 'utf-8)
  :config
  (spaceline-spacemacs-theme)
  ;; (spaceline-helm-mode)
  ;; (spaceline-info-mode)
  )

(defvar jco/cursor-color)
(set (make-local-variable 'jco/cursor-color) "green")

(load-theme jco/theme t)

(cl-case jco/theme
  (darkane
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "midnightblue")
   (set-face-background 'hl-line "#041040"))

  (gotham
   (setq jco/cursor-color "lightblue")
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "RoyalBlue4"))

  (material
   (set-face-background 'hl-line "#37474f")
   (set-face-background 'org-todo nil))

  (material-light
   (setq jco/cursor-color "azure4")
   (set-face-background 'hl-line "Gray75"))

  (meacupla
   (setq jco/cursor-color "azure4")
   (set-face-background 'hl-line "Gray80")
   (set-face-background 'whitespace-trailing "IndianRed1"))

  (minimal
   (set-face-background 'hl-line "#101f24")
   (set-face-background 'org-todo nil)
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "DarkOrange4"))

  (molokai
   (set-face-foreground 'font-lock-comment-face "azure4")
   (set-face-background
    'evil-search-highlight-persist-highlight-face "RoyalBlue4")
   )

  (organic-green
   (setq jco/cursor-color "gray25")
   (set-face-background 'helm-selection "#a0f0a0")
   (set-face-background 'show-paren-match "#c0c060"))

  ((sanityinc-tomorrow-bright sanityinc-tomorrow-day
                              sanityinc-tomorrow-eighties
                              sanityinc-tomorrow-night)
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "RoyalBlue"))

  (sanityinc-tomorrow-blue
   (setq jco/cursor-color "snow")
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "RoyalBlue")
   (set-face-background 'company-tooltip-selection "snow")
   (set-face-foreground 'company-tooltip-selection "gray8")
   (set-face-foreground 'company-tooltip-common-selection "VioletRed4")
   (set-face-background 'company-scrollbar-fg "LightBlue"))

  (soothe
   (require 'linum)
   (set-face-foreground 'linum "gray35"))

  (tao-yang
   (setq jco/cursor-color "azure4"))

  (tao-yin
   (setq jco/cursor-color "gray50")
   (set-face-background 'hl-line "gray16")
   (set-face-foreground 'linum "gray25")
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "MidnightBlue")))

(set-face-foreground 'minibuffer-prompt "#263238")
(set-face-background 'minibuffer-prompt "#afd700")

;; Fix ediff colors.
(require 'ediff)
(dolist (f '(ediff-current-diff-A
             ediff-current-diff-Ancestor
             ediff-current-diff-B
             ediff-current-diff-C
             ediff-even-diff-A
             ediff-even-diff-Ancestor
             ediff-even-diff-B
             ediff-even-diff-C
             ediff-fine-diff-A
             ediff-fine-diff-Ancestor
             ediff-fine-diff-B
             ediff-fine-diff-C
             ediff-odd-diff-A
             ediff-odd-diff-Ancestor
             ediff-odd-diff-B
             ediff-odd-diff-C))
  (set-face-foreground f "black"))

(set-face-background 'info-function-ref-item "gray16")
(set-face-background 'info-variable-ref-item "gray16")
(set-face-background 'info-user-option-ref-item "gray16")
(set-face-background 'info-reference-item "gray16")

(require 'evil)
(setq evil-normal-state-cursor `(,jco/cursor-color box))
(setq evil-insert-state-cursor `(,jco/cursor-color bar))
(blink-cursor-mode -1)

(provide 'init-theme)

;;; init-theme.el ends here
