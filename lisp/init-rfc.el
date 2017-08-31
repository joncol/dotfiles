(setq auto-mode-alist
      (cons '("/rfc[0-9]+\\.txt\\(\\.gz\\)?\\'" . rfcview-mode)
            auto-mode-alist))

(autoload 'rfcview-mode "rfcview" nil t)

(add-hook 'rfcview-mode-hook
          (lambda ()
            (set-face-foreground 'rfcview-rfcnum-face "turquoise")
            (set-face-foreground 'rfcview-title-face "OrangeRed1")
            (set-face-foreground 'rfcview-headname-face "chartreuse")
            (set-face-foreground 'rfcview-headnum-face "chartreuse4")))

(provide 'init-rfc)
