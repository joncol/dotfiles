;;; #init-company.el --- Company config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)

  :bind (([C-iso-lefttab] . company-ispell)
         :map company-active-map
         ("C-j" . company-select-next-or-abort)
         ("C-k" . company-select-previous-or-abort)
         ("C-n" . company-select-next-or-abort)
         ("C-p" . company-select-previous-or-abort)
         ("C-d" . company-show-doc-buffer)
         ("M-." . company-show-location))

  :config
  (add-to-list 'completion-styles 'initials t)
  (setq company-tooltip-align-annotations t)
  (setq company-dabbrev-ignore-case 'keep-prefix)
  (setq company-dabbrev-code-ignore-case nil)
  (setq company-dabbrev-downcase nil)

  (setq company-tooltip-limit 20)
  (setq company-idle-delay .3)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command)))

(use-package company-lsp)

(defvar-local jco/prev-fci-status nil)

;; Fixes the issue where the first item is shown far off to the right.

(defun jco/turn-off-fci(&rest ignore)
  (when (boundp 'fci-mode)
    (setq jco/prev-fci-status fci-mode)
    (when fci-mode (fci-mode -1))))

(defun jco/maybe-turn-on-fci(&rest ignore)
  (when jco/prev-fci-status
    (fci-mode 1)))

(add-hook 'company-completion-started-hook 'jco/turn-off-fci)
(add-hook 'company-completion-finished-hook 'jco/maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'jco/maybe-turn-on-fci)
(add-hook 'evil-insert-state-exit-hook 'jco/maybe-turn-on-fci)

(provide 'init-company)

;;; init-company.el ends here
