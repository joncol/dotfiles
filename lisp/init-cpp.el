;;; #init-cpp.el --- C++ config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(require 'init-common-funs)

(when (jco/at-office-p)
    (add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)))

(require 'cc-align)
(require 'cc-defs)

(defun inside-class-enum-p (pos)
  "Check if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum[ \t]+class[ \t]+[^}]+" nil))))

(defun align-enum-class (langelem)
  "Align LANGELEM inside C++11 enum class."
  (if (inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun align-enum-class-closing-brace (langelem)
  "Align LANGELEM inside C++11 enum class."
  (if (inside-class-enum-p (c-langelem-pos langelem))
      '-
    '+))

(require 'cc-vars)

(defun fix-enum-class ()
  "Setup `c++-mode' to better handle \"class enum\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
  (add-to-list 'c-offsets-alist
               '(statement-cont . align-enum-class-closing-brace)))

(defadvice c-lineup-arglist (around my activate)
  "Improve indentation of continued C++11 lambda function opened as argument."
  (setq ad-return-value
        (if (and (equal major-mode 'c++-mode)
                 (ignore-errors
                   (save-excursion
                     (goto-char (c-langelem-pos langelem))
                     (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
            0
          ad-do-it)))

(require 'cc-mode)
(require 'flycheck)

(add-hook 'c++-mode-hook
          (lambda ()
            (let ((sh (getenv "SHELL")))
              (set (make-local-variable 'compile-command)
                   (concat
                    "cd " (projectile-project-root)
                    (cond
                     ((s-contains-p "bash" sh)
                      "_build_vs && cmake --build . -- -j4")
                     ((s-contains-p "fish" sh)
                      (format "_build ;and cmake --build . --target %s -- -j4"
                              (jco/cmake-project-name)))))))

            (jco/define-bindings c++-mode-map '(("<f6>" . compile)))

            (setq flycheck-clang-language-standard "c++14")

            (c-set-offset 'innamespace 0)
            (c-set-offset 'label '-)
            (fix-enum-class)

            (evil-leader/set-key "q i" 'jco/fix-constr-destr)
            (evil-leader/set-key "q d" 'jco/decl-to-def)
            (evil-leader/set-key "q D" 'jco/def-to-decl)
            (evil-leader/set-key "q r" 'jco/make-const-ref)
            (evil-leader/set-key "q t" 'jco/variadic-templatize)
            (evil-leader/set-key "q c" 'jco/fix-class-name)
            (evil-leader/set-key "q o" 'jco/override-cpp-method)

            (evil-leader/set-key "q C"
              (lambda ()
                (interactive)
                (jco/insert-class-name)
                (insert "::")))))

(provide 'init-cpp)

;;; init-cpp.el ends here
