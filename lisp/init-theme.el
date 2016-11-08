(setq jco/theme 'minimal) ;;; change this to whatever theme you want

(require 'airline-themes)

(if (eq system-type 'darwin)
    (nyan-mode)
  (load-theme 'airline-powerlineish t)) ;;; airline is too slow on Mac OS X

(setq jco/cursor-color "green")

(if (display-graphic-p)
    (progn
      (load-theme jco/theme t)

      (case jco/theme
        ('gotham
         (setq jco/cursor-color "lightblue")
         (set-face-background 'evil-search-highlight-persist-highlight-face
                              "RoyalBlue4"))

        ('material
         (set-face-background 'hl-line "#37474f")
         (set-face-background 'org-todo nil))

        ('molokai
         (set-face-foreground 'font-lock-comment-face "azure4")
         (set-face-background
          'evil-search-highlight-persist-highlight-face "RoyalBlue4")
         )

        ('organic-green
         (setq jco/cursor-color "gray25")
         (set-face-background 'helm-selection "#a0f0a0")
         (set-face-background 'show-paren-match "#c0c060"))

        ('soothe
         (require 'linum)
         (set-face-foreground 'linum "gray35")))

      (set-face-foreground 'minibuffer-prompt "#263238")
      (set-face-background 'minibuffer-prompt "#afd700"))

  (load-theme 'cyberpunk t))

(setq evil-normal-state-cursor `(,jco/cursor-color box))
(setq evil-insert-state-cursor `(,jco/cursor-color bar))
(blink-cursor-mode -1)

(provide 'init-theme)
