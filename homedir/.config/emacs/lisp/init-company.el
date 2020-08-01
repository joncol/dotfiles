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
         ("M-." . company-show-location)
         ("RET" . company-complete-selection))

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

(use-package company-box
  :if (display-graphic-p)
  :hook (company-mode . company-box-mode))

(provide 'init-company)

;;; init-company.el ends here
