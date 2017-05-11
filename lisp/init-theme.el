;;; #init-theme.el --- Theme settings -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

;;; Change this to whatever theme you want.
(defvar jco/theme)
(set (make-local-variable 'jco/theme)
     ;; Change this to whatever theme you want.
     ;; 'adwaita
     ;; 'darktooth
     ;; 'gotham
     ;; 'molokai
     ;; 'organic-green
     ;; 'reykjavik
     'sanityinc-tomorrow-blue
     ;; 'sanityinc-tomorrow-eighties
     ;; 'sanityinc-tomorrow-night
     ;; 'solarized
     )

(defvar jco/theme-packages)

(set (make-local-variable 'jco/theme-packages)
     '(afternoon-theme
       ample-theme
       ample-zen-theme
       borland-blue-theme
       cherry-blossom-theme
       color-theme-sanityinc-tomorrow
       cyberpunk-theme
       darkane-theme
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
       prassee-theme
       reykjavik-theme
       solarized-theme
       soothe-theme
       tao-theme))

(dolist (p jco/theme-packages)
  (unless (package-installed-p p)
    (package-install p)))

(use-package powerline)

(use-package spaceline
  :init
  (require 'spaceline-config)
  (setq powerline-default-separator 'utf-8)
  (spaceline-spacemacs-theme))

(use-package spaceline-all-the-icons
  :if (not (eq system-type 'windows-nt))
  :after spaceline
  :config
  (spaceline-all-the-icons-theme)
  (setq spaceline-all-the-icons-separator-type 'arrow))

(load-theme jco/theme t)

(defvar jco/cursor-color)
(set (make-local-variable 'jco/cursor-color) "green")

(set-face-background 'evil-search-highlight-persist-highlight-face
                     "RoyalBlue4")

(cl-case jco/theme
  (adwaita
   (setq jco/cursor-color  "#101f24"))

  (darkane
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "midnightblue")
   (set-face-background 'hl-line "#041040"))

  (gotham
   (setq jco/cursor-color "lightblue"))

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
   (set-face-foreground 'font-lock-comment-face "azure4"))

  (organic-green
   (setq jco/cursor-color "gray25")
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "#7ceece")
   (set-face-background 'helm-selection "#a0f0a0")
   (set-face-background 'show-paren-match "#c0c060"))

  (prassee
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "VioletRed4"))

  (reykjavik
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "VioletRed4"))

  ((sanityinc-tomorrow-bright sanityinc-tomorrow-day
                              sanityinc-tomorrow-eighties
                              sanityinc-tomorrow-night)
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "RoyalBlue"))

  (sanityinc-tomorrow-blue
   (setq jco/cursor-color "snow")
   (set-face-background 'helm-selection "VioletRed4")
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

(when (not (eq jco/theme 'cyberpunk))
  (let ((info-bg "gray16"))
    (dolist (f '(info-command-ref-item
                 info-constant-ref-item
                 info-file
                 info-function-ref-item
                 info-macro-ref-item
                 info-reference-item
                 info-special-form-ref-item
                 info-syntax-class-item
                 info-user-option-ref-item
                 info-variable-ref-item))
      (set-face-background f info-bg))))

(set-face-background 'helm-buffer-directory "gray60")

(require 'evil-states)
(setq evil-normal-state-cursor `(,jco/cursor-color box))
(setq evil-insert-state-cursor `(,jco/cursor-color bar))
(blink-cursor-mode -1)

(provide 'init-theme)

;;; init-theme.el ends here
