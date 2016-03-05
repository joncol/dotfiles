(require 'semantic)

;; (add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defun inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum[ \t]+class[ \t]+[^}]+"))))

(defun align-enum-class (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun align-enum-class-closing-brace (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      '-
    '+))

(defun fix-enum-class ()
  "Setup `c++-mode' to better handle \"class enum\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
  (add-to-list 'c-offsets-alist
               '(statement-cont . align-enum-class-closing-brace)))

(defadvice c-lineup-arglist (around my activate)
  "Improve indentation of continued C++11 lambda function opened as argument"
  (setq ad-return-value
        (if (and (equal major-mode 'c++-mode)
                 (ignore-errors
                   (save-excursion
                     (goto-char (c-langelem-pos langelem))
                     (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
            0
          ad-do-it)))

(add-hook 'c++-mode-hook
          (lambda ()
            (setq compile-command
                  (if (jco/at-office-p)
                      (let ((sln-file
                             (car (directory-files (projectile-project-root)
                                                   nil ".*\.sln"))))
                        (concat "cd " (projectile-project-root)
                                " && msbuild " sln-file " /p:UseEnv=true"))
                    (concat "cd " (projectile-project-root)
                            "debug ;and make -j4 ;and ctest")))
            (jco/define-bindings c++-mode-map
                                 '(("<f6>" . compile)))
            (c-set-offset 'innamespace 0)
            (c-set-offset 'label '-)
            (fix-enum-class)))

(provide 'init-cpp)
