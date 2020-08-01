;;; #init-nxml.el --- nxml config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(add-to-list 'auto-mode-alist '("\\.qrc\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rnc\\'" . rnc-mode))

(defun jco/rng-reload-schema ()
  "Reload current XML schema."
  (interactive)
  (let ((schema-filename rng-current-schema-file-name))
    (when schema-filename
      (setq rng-current-schema (rng-load-schema schema-filename))
      (run-hooks 'rng-schema-change-hook)
      (message "Reloaded schema %s" schema-filename))
    (unless schema-filename
      (rng-set-schema-and-validate))))

(defun jco/xmllint-format-buffer ()
  "Format XML buffer, using xmllint."
  (interactive)
  (save-restriction
    (widen)
    (shell-command-on-region (point-min) (point-max)
                             "xmllint --format -" t t)))

(add-hook 'nxml-mode-hook
          (lambda ()
            (setq evil-shift-width 2)
            (setq rnc-indent-level 2)
            (jco/define-bindings nxml-mode-map
                                 '(("C-c C-p" . rng-previous-error)
                                   ("C-c C-r" . jco/rng-reload-schema)))
            (add-to-list 'rng-schema-locating-files
                         (expand-file-name (concat user-emacs-directory
                                                   "xslt-relax-ng/schemas.xml")))
            (when (eq system-type 'windows-nt)
              (setq rnc-jing-jar-file "c:/tools/jing/bin"))
            (evil-leader/set-key "x l" 'jco/xmllint-format-buffer)))

(provide 'init-nxml)

;;; init-nxml.el ends here
