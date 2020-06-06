;;; #init-cc.el --- C/C++ config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (eq system-type 'windows-nt)
              (setq company-backends (delete 'company-clang company-backends))
              (global-set-key "\M-." 'ggtags-find-tag-dwim)
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                (ggtags-mode)
                (jco/define-bindings ggtags-mode-map
                                     '(("C-c t s" . ggtags-find-other-symbol)
                                       ("C-c t h" . ggtags-view-tag-history)
                                       ("C-c t r" . ggtags-find-reference)
                                       ("C-c t c" . ggtags-create-tags)
                                       ("C-c t u" . ggtags-update-tags)
                                       ("M-,"     . pop-tag-mark)))))
            ;; (add-hook 'compilation-mode-hook
            ;;           (lambda ()
            ;;             (when (not (get-buffer-window "*compilation*"))
            ;;               (save-selected-window
            ;;                 (save-excursion
            ;;                   (jco/select-bottom-window)
            ;;                   (let* ((w (split-window-vertically))
            ;;                          (h (window-height w)))
            ;;                     (select-window w)
            ;;                     (switch-to-buffer "*compilation*")
            ;;                     (shrink-window
            ;;                      (- h (or compilation-window-height 20)))))))))
            (setq-default backward-delete-function nil)
            (c-add-style "my-c-style"
                         '((c-basic-offset . 4)
                           (c-offsets-alist (label . +))))
            (c-set-style "my-c-style")
            (c-set-offset 'substatement-open 0)
            (c-set-offset 'inline-open 0)
            (setq tab-width 4)
            (setq align-to-tab-stop nil)
            (c-set-offset 'substatement-open 0)
            (global-ede-mode t)
            (ede-enable-generic-projects)
            (evil-leader/set-key "a"
              (lambda ()
                (interactive)
                (ff-find-other-file nil t)))
            (evil-leader/set-key "A"
              (lambda ()
                (interactive)
                (ff-find-other-file t t)))
            (smartparens-mode)))

(provide 'init-cc)

;;; init-cc.el ends here
