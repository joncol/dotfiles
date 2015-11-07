(defun neotree-mode-hook ()
  (hl-line-mode 1)
  (jco/define-bindings evil-normal-state-local-map
                       '(("TAB" . neotree-enter)
                         ("SPC" . neotree-enter)
                         ("RET" . neotree-enter)
                         ("c" . neotree-change-root)
                         ("g" . neotree-refresh)
                         ("q" . neotree-hide)
                         ("v" . neotree-enter-vertical-split)
                         ("s" . neotree-enter-horizontal-split))))

(provide 'init-neotree)
