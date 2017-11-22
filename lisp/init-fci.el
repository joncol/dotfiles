;;; #init-fci.el --- Config for fci-mode -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package fill-column-indicator
  :config
  (setq-default fill-column 80)
  (setq fci-rule-color "#ff0000")
  (setq fci-rule-character-color "#ff0000")
  (setq fci-always-use-textual-rule 1))

;;; Fix for htmlize producing garbage newlines when using fci-mode.

(defvar modi/htmlize-initial-fci-state nil
  "Variable to store the state of `fci-mode' when `htmlize-buffer' is called.")

(defvar modi/htmlize-initial-flyspell-state nil
  "Variable to store the state of `flyspell-mode' when `htmlize-buffer' is
called.")

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

(defun disable-fci-temporarily (orig-fun &rest args)
  "Disable `fci-mode' before calling ORIG-FUN; re-enable afterwards."
  (let ((fci-was-initially-on (when fci-mode
                                (prog1
                                    fci-mode
                                  (fci-mode -1)))))
    (prog1
        (apply orig-fun args)
      (when fci-was-initially-on
        (fci-mode 1)))))

(advice-add 'shell-command :around #'disable-fci-temporarily)
(advice-add 'shell-command-on-region :around #'disable-fci-temporarily)

(provide 'init-fci)

;;; init-fci.el ends here
