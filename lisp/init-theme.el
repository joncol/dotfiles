(if (eq system-type 'darwin)
    (nyan-mode)
  (load-theme 'airline-powerlineish t)) ;;; airline is too slow on Mac OS X

(setq jco/cursor-color "green")

(if (display-graphic-p)
    (let ((theme 'organic-green)) ;;; change this to whatever theme you want
      (load-theme theme t)

      (case theme
        ('material
         (set-face-background 'hl-line "#37474f")
         (set-face-foreground 'minibuffer-prompt "#263238"))

        ('molokai
         (set-face-foreground 'font-lock-comment-face "azure4")
         (set-face-background
          'evil-search-highlight-persist-highlight-face "RoyalBlue4"))

        ('organic-green
         (setq jco/cursor-color "gray25")
         (set-face-background 'helm-selection "#a0f0a0"))

        ('soothe
         (require 'linum)
         (set-face-foreground 'linum "gray35"))))

  (load-theme 'cyberpunk t))

(setq evil-normal-state-cursor `(,jco/cursor-color box))
(setq evil-insert-state-cursor `(,jco/cursor-color bar))

(provide 'init-theme)
