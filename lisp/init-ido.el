(ido-mode)
(flx-ido-mode)
;; (setq ido-enable-flex-matching t)

;;; display ido results vertically, rather than horizontally
(setq ido-decorations '("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]"
                        " [Matched]" " [Not readable]" " [Too big]"
                        " [Confirm]"))
(defun ido-disable-line-truncation ()
  (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
(add-hook 'ido-setup-hook
          'jco/define-bindings ido-completion-map '(("C-n" . ido-next-match)
                                                    ("C-p" . ido-prev-match)))

(provide 'init-ido)
