(require 'fill-column-indicator)

(setq-default fill-column 80)
(setq fci-rule-color "#ff0000")
(setq fci-always-use-textual-rule 1)

;;; Fix for htmlize producing garbage newlines when using fci-mode.

(defvar modi/htmlize-initial-fci-state nil
  "Variable to store the state of `fci-mode' when `htmlize-buffer' is called.")
(defvar modi/htmlize-initial-flyspell-state nil
  "Variable to store the state of `flyspell-mode' when `htmlize-buffer' is called.")

(defun modi/htmlize-before-hook-fn ()
  (when (fboundp 'fci-mode)
    (setq modi/htmlize-initial-fci-state fci-mode)
    (when fci-mode
      (fci-mode -1)))
  (when (fboundp 'flyspell-mode)
    (setq modi/htmlize-initial-flyspell-state flyspell-mode)
    (when flyspell-mode
      (flyspell-mode -1))))
(add-hook 'htmlize-before-hook #'modi/htmlize-before-hook-fn)

(defun modi/htmlize-after-hook-fn ()
  (when (fboundp 'fci-mode)
    (when modi/htmlize-initial-fci-state
      (fci-mode 1)))
  (when (fboundp 'flyspell-mode)
    (when modi/htmlize-initial-flyspell-state
      (flyspell-mode 1))))
(add-hook 'htmlize-after-hook #'modi/htmlize-after-hook-fn)

(provide 'init-fci)
