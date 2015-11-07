(cond
 ((and (eq system-type 'windows-nt) (display-graphic-p))
  (set-frame-font "Inconsolata-12")
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 100 60))

 ((and (eq system-type 'gnu/linux) (display-graphic-p))
  (set-frame-font "Bitstream Vera Sans Mono")
  (set-face-attribute 'default nil :height 105)
  (set-frame-size (selected-frame) 93 64))

 ((eq system-type 'darwin)
  (setq mac-right-option-modifier 'none)
  (when (display-graphic-p)
    (set-frame-size (selected-frame) 93 60)
    (set-face-attribute 'default nil :height 145))))

(provide 'init-font)
