(if (eq system-type 'darwin)
    (nyan-mode)
  (load-theme 'airline-powerlineish t)) ;;; airline is too slow on Mac OS X

(setq jco/cursor-color "green")

(if (display-graphic-p)
    (let ((theme 'organic-green))
      (load-theme theme t)

      (case theme

        ('molokai
         (set-face-foreground 'font-lock-comment-face "azure4")
         (set-face-background
          'evil-search-highlight-persist-highlight-face "RoyalBlue4"))

        ('soothe
         (require 'linum)
         (set-face-foreground 'linum "gray35"))

        ('organic-green
         (setq jco/cursor-color "gray25")
         (set-face-background 'helm-selection "#a0f0a0"))))

  (load-theme 'cyberpunk t))

(setq evil-normal-state-cursor `(,jco/cursor-color box))
(setq evil-insert-state-cursor `(,jco/cursor-color bar))

(provide 'init-theme)
