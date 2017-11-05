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
     ;; 'kaolin-dark
     ;; 'kaolin-eclipse
     ;; 'kaolin-ocean
     ;; 'molokai
     ;; 'mustang
     ;; 'nubox-dark
     'nubox-light
     ;; 'organic-green
     ;; 'reykjavik
     ;; 'sanityinc-tomorrow-blue
     ;; 'sanityinc-tomorrow-eighties
     ;; 'sanityinc-tomorrow-night
     ;; 'solarized-dark
     ;; 'solarized-light
     ;; 'tao-yin
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
       ;; darkane-theme
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
       kaolin-themes
       leuven-theme
       material-theme
       minimal-theme
       molokai-theme
       monokai-theme
       mustang-theme
       nubox
       organic-green-theme
       prassee-theme
       reykjavik-theme
       solarized-theme
       soothe-theme
       tao-theme))

(dolist (p jco/theme-packages)
  (unless (package-installed-p p)
    (package-install p)))

(use-package powerline
  :disabled t)

(use-package spaceline
  :disabled t
  :init
  (require 'spaceline-config)
  (setq powerline-default-separator 'arrow)
  (spaceline-spacemacs-theme))

(load-theme jco/theme t)

;; (defvar jco/cursor-color)
;; (set (make-local-variable 'jco/cursor-color) "green")

(set-face-background 'evil-search-highlight-persist-highlight-face
                     "RoyalBlue4")

(set-face-foreground 'show-paren-match "#101f24")
(set-face-background 'show-paren-match "#89C5B7")

(cl-case jco/theme
  (adwaita
   (set-face-background 'hl-line "#dadfe1")
   (set-face-background 'evil-search-highlight-persist-highlight-face "#e0dcbe")
   (add-hook 'smartparens-mode-hook
             #'(lambda ()
                 (set-face-background 'sp-pair-overlay-face "LightBlue")))
   (setq jco/cursor-color  "#101f24")
   (when (featurep 'mu4e)
     (set-face-background 'mu4e-highlight-face "#7ceece")
     (set-face-foreground 'mu4e-header-highlight-face "#101f24")))

  (darkane
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "midnightblue")
   (set-face-background 'hl-line "#041040"))

  (gotham
   (setq jco/cursor-color "lightblue")
   (set-face-background 'evil-search-highlight-persist-highlight-face "#e0dcbe")
   (set-face-foreground 'evil-search-highlight-persist-highlight-face "#101f24")
   )

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
   (add-hook 'smartparens-mode-hook
             #'(lambda ()
                 (set-face-background 'sp-pair-overlay-face "#582c6b")))
   (set-face-background 'swiper-line-face "#582c6b")
   (set-face-background 'region "#582c6b")
   (set-face-background 'evil-search-highlight-persist-highlight-face "#f9bf3b")
   (set-face-foreground 'evil-search-highlight-persist-highlight-face "#465457")
   (set-face-background 'lazy-highlight "#f9bf3b")
   (set-face-background 'ffap "#582c6b")
   (when (featurep 'mu4e)
     (set-face-background 'mu4e-highlight-face "#582c6b")
     (set-face-foreground 'mu4e-header-highlight-face "#101f24")))

  (mustang
   (setq jco/cursor-color "#ececec")
   (set-face-background 'region "#582c6b")
   (set-face-background 'lazy-highlight "VioletRed3")
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "#e0dcbe")
   (set-face-foreground 'evil-search-highlight-persist-highlight-face
                        "#101f24")
   (set-face-foreground 'font-lock-warning-face "#ff6523")
   (set-face-background 'font-lock-warning-face nil)
   (set-face-background 'ivy-minibuffer-match-face-2 "#582c6b")
   (set-face-background 'vhl/default-face "#582c6b"))

  (nubox-dark
   (set-face-background 'hl-line "#2a2d2e")
   (set-face-background 'swiper-line-face "#2a2d2e")
   (set-face-background 'vhl/default-face "#2a2d2e")
   (set-face-background 'iedit-occurrence "#2a2d2e")
   (add-hook 'smartparens-mode-hook
             #'(lambda ()
                 (set-face-background 'sp-pair-overlay-face "#444748")))
   (set-face-background 'region "#582c6b")
   (set-face-background 'ivy-minibuffer-match-face-2 "#444748")
   (set-face-background 'ffap "#582c6b")
   (set-face-background 'highlight "#582c6b"))

  (nubox-light
   (setq jco/cursor-color "#101f24")
   (set-face-background 'swiper-line-face "#e0dcbe")
   (set-face-background 'hl-line "#e0dcbe")
   (set-face-background 'vhl/default-face "#e0dcbe")
   (add-hook 'smartparens-mode-hook
             #'(lambda ()
                 (set-face-background 'sp-pair-overlay-face "#c7c3a5")))
   (set-face-background 'region "#ffc3ff")
   (set-face-background 'ffap "#ffc3ff")
   (set-face-background 'evil-search-highlight-persist-highlight-face "#f9bf3b")

   (add-hook 'ledger-mode-hook
             #'(lambda ()
                 (set-face-background 'ledger-font-xact-highlight-face
                                      "#ffc3ff")
                 (set-face-background 'ledger-occur-xact-face "#ffc3ff")))

   (when (featurep 'mu4e)
     (set-face-background 'mu4e-highlight-face "#7ceece")
     (set-face-foreground 'mu4e-header-highlight-face "#101f24")))

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
   (setq jco/cursor-color  "#7ceece")
   (set-face-background 'region "#1a4550")
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "#821800"))

  ((sanityinc-tomorrow-bright sanityinc-tomorrow-day
                              sanityinc-tomorrow-eighties
                              sanityinc-tomorrow-night)
   (setq jco/cursor-color "snow")
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "RoyalBlue4"))

  (sanityinc-tomorrow-blue
   (setq jco/cursor-color "snow")
   (set-face-background 'helm-selection "VioletRed4")
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "RoyalBlue")
   (set-face-background 'company-tooltip-selection "snow")
   (set-face-foreground 'company-tooltip-selection "gray8")
   (set-face-foreground 'company-tooltip-common-selection "VioletRed4")
   (set-face-background 'company-scrollbar-fg "LightBlue"))

  (solarized-dark
   (set-face-background 'region "#1a4550")
   (set-face-background 'swiper-line-face "#335e69")
   (set-face-background 'ivy-current-match "#335e69"))

  (solarized-light
   (setq jco/cursor-color "gray25")
   (set-face-background 'region "#e0dcbe")
   (set-face-background 'cider-deprecated-face "#e0dcbe")
   (set-face-background 'evil-search-highlight-persist-highlight-face "#f9bf3b")
   (set-face-background 'lazy-highlight "#f9bf3b"))

  (soothe
   (require 'linum)
   (set-face-foreground 'linum "gray35"))

  (tao-yang
   (setq jco/cursor-color "azure4"))

  (tao-yin
   (set-face-background 'region "#4a3f51")
   (setq jco/cursor-color "gray50")
   (set-face-background 'hl-line "gray16")
   (set-face-foreground 'linum "gray25")
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "MidnightBlue")))

;;; Make syntax highlighting work also for current line.
(set-face-foreground 'highlight nil)

;;; ... And selected region.
(set-face-foreground 'region nil)

(set-face-foreground 'minibuffer-prompt "#263238")
(set-face-background 'minibuffer-prompt "#afd700")

;; Fix annoyingly dark backgrounds of dired-subtree faces.
(add-hook 'dired-mode-hook
          #'(lambda ()
              (let* ((ns (number-sequence 1 5))
                     (f  #'(lambda (x)
                             (intern (format "dired-subtree-depth-%d-face" x))))
                     (ss (map 'cons f ns)))
                (dolist (f ss)
                  (set-face-background f nil)))))

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

(when (boundp 'jco/cursor-color)
  (require 'evil-states)
  (setq evil-normal-state-cursor `(,jco/cursor-color box))
  (setq evil-insert-state-cursor `(,jco/cursor-color bar)))

(blink-cursor-mode -1)

(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

(provide 'init-theme)

;;; init-theme.el ends here
