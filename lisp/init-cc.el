(add-hook 'c-mode-common-hook
          (lambda ()
            (when (not (eq system-type 'gnu/linux))
              (setq company-backends (delete 'company-clang company-backends))

              (helm-gtags-mode)

              (with-eval-after-load 'evil
                (define-key evil-normal-state-map (kbd "M-.") nil))

              (global-set-key "\M-." 'ggtags-find-tag-dwim)

              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                (ggtags-mode)
                (diminish 'ggtags-mode)
                (jco/define-bindings ggtags-mode-map
                                     '(("C-c t s" . ggtags-find-other-symbol)
                                       ("C-c t h" . ggtags-view-tag-history)
                                       ("C-c t r" . ggtags-find-reference)
                                       ("C-c t c" . ggtags-create-tags)
                                       ("C-c t u" . ggtags-update-tags)
                                       ("M-," . pop-tag-mark)))))

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

            (evil-leader/set-key "q i" 'jco/fix-constr-destr)
            (evil-leader/set-key "q d" 'jco/decl-to-def)
            (evil-leader/set-key "q D" 'jco/def-to-decl)
            (evil-leader/set-key "q r" 'jco/make-const-ref)
            (evil-leader/set-key "q t" 'jco/variadic-templatize)
            (evil-leader/set-key "q c" 'jco/fix-class-name)

            (evil-leader/set-key "q C"
              (lambda ()
                (interactive)
                (jco/insert-class-name)
                (insert "::")))
            ))

(provide 'init-cc)
