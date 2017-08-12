(require 'nxml-mode)

(setq rnc-indent-level 2)

(define-key nxml-mode-map (kbd "C-c C-p") 'rng-previous-error)

(add-to-list 'auto-mode-alist '("\\.qrc\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rnc\\'" . rnc-mode))
(add-to-list 'rng-schema-locating-files
             (expand-file-name (concat user-emacs-directory
                                       "xslt-relax-ng/schemas.xml")))

(when (eq system-type 'windows-nt)
  (setq rnc-jing-jar-file "c:/tools/jing/bin"))

(define-key nxml-mode-map (kbd "C-c C-r") 'jco/rng-reload-schema)

(defun jco/rng-reload-schema ()
  (interactive)
  (let ((schema-filename rng-current-schema-file-name))
    (when schema-filename
      (setq rng-current-schema (rng-load-schema schema-filename))
      (run-hooks 'rng-schema-change-hook)
      (message "Reloaded schema %s" schema-filename))
    (unless schema-filename
      (rng-set-schema-and-validate))))

(defun jco/xmllint-format-buffer ()
  (interactive)
  (save-restriction
    (widen)
    (shell-command-on-region (point-min) (point-max)
                             "xmllint --format -" t t)))

(add-hook 'nxml-mode-hook
          (lambda ()
            (setq evil-shift-width 2)
            (smartparens-mode)
            (evil-leader/set-key "x f" 'jco/xmllint-format-buffer)))

(provide 'init-nxml)
