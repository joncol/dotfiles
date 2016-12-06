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

(fset 'jco/cpp-fix-constr-destr
      [?V ?j ?= ?\M-x ?j ?c ?o ?/ ?c ?p ?p ?- ?i ?n ?s ?e ?r ?t ?- ?c ?l ?a ?s
          ?s ?- ?n ?a ?m ?e return ?i ?: ?: ?k ?j ?f ?\( ?l ?\" ?t ?y ?i ?w ?f
          ?= ?h ?d ?t ?\) ?l ?s return ?\{ backspace ?: ? ?\C-r ?t ?\( ?p ?a ?r
          ?e ?n ?t ?k ?j ?o ?\{ return ?k ?j ?d ?d ?o ?k ?j ?j ?d ?w ?\M-x ?j ?c
          ?o ?/ ?c ?p ?p ?- ?i ?n ?s ?e ?r ?t ?- ?c ?l ?a ?s ?s ?- ?n ?a ?m ?e
          return ?i ?: ?: ?k ?j ?f ?\) ?a ? ?= ? ?d ?e ?f ?a ?u ?l ?t ?k ?j ?j])

(fset 'jco/cpp-def-to-decl
      [?V ?/ ?\) return ?= ?2 ?f ?: ?l ?d ?B ?/ ?\) return ?A ?\; ?k ?j ?0
          ?\C-x ?\ ])

(fset 'jco/cpp-variadic-templatize
      [?0 ?O ?t ?e ?m ?p ?l ?a ?t ?e ?< ?t ?y ?p ?e ?n ?a ?m ?e ?. ?. ?. ?\S- ?T
          ?s ?> ?k ?j ?0 ?j ?f ?\( ?l ?c ?i ?\) ?T ?s ?& ?& ?. ?. ?. ? ?a ?r ?g
          ?s ?k ?j ?0 ?j])

(fset 'jco/cpp-make-const-ref "iconst kjEa&kj")

(fset 'jco/cpp-fix-class-name
      [?d ?i ?w ?\M-x ?j ?c ?o ?/ ?c ?p ?p ?- ?i ?n ?s ?e ?r ?t ?- ?c ?l ?a
          ?s ?s ?- ?n ?a ?m ?e return])

(fset 'jco/cpp-override-method
      [?0 ?w ?d ?w ?/ ?\; return ?: ?s ?/ ?\\ ?\( ? ?= ? ?0 ?\\ ?\) ?? ?\; ?/
          ? ?o ?v ?e ?r ?r ?i ?d ?e ?\; ?/ return ?\C-o ?\C-o ?0 ?\C-x ?\ ])

(defun jco/cpp-decl-field-accessors ()
  "Create C++ accessor declarations from field."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (re-search-forward "\\(\\s-+\\)\\(.+\\) m_\\(.+\\);" (point-at-eol) nil)
    (let* ((whole (match-string 0))
           (name (match-string 3))
           (uname (save-match-data
                    (string-inflection-camelcase-function name)))
           (lname (save-match-data
                    (string-inflection-lower-camelcase-function name)))
           (spc (match-string 1))
           (type (match-string 2)))
      (replace-match (format "%s\n%svoid set%s(%s %s);\n%s%s %s(%s %s) const;"
                             whole spc uname type name
                             spc type lname type name)))))

(defun jco/cpp-decl-to-def ()
  "Create C++ method definition from declaration."
  (interactive)
  (defvar jco/start-pos)
  (set (make-local-variable 'jco/start-pos) (point))
  (save-excursion
    (save-restriction
      (widen)
      (beginning-of-line)
      (re-search-forward ";")
      (defvar jco/end-of-decl)
      (set (make-local-variable 'jco/end-of-decl) (point))
      (goto-char jco/start-pos)

      (save-excursion (while (re-search-forward "\\(Q_INVOKABLE \\| override\\)"
                                 jco/end-of-decl t)
         (replace-match "")))

      (re-search-forward "\\([^\s-]+\\)(.+ \\(.+\\))" jco/end-of-decl nil)
      (let* ((method-name (match-string 1))
             (param-name (match-string 2))
             (method-name-t (save-match-data
                              (string-inflection-underscore-function
                               method-name)))
             (is-setter (string= method-name-t (concat "set_" param-name)))
             (is-getter (string= method-name-t param-name)))
        (goto-char (match-beginning 0))
        (jco/cpp-insert-class-name)
        (insert "::")
        (re-search-forward ";")
        (goto-char (match-beginning 0))
        (replace-match "\n{\n")
        (cond (is-setter (insert (format "m_%s = %s;\n" param-name param-name)))
              (is-getter (insert (format "return m_%s;\n" param-name))))
        (insert "}")
        (evil-indent jco/start-pos (point))))))

(add-hook 'c++-mode-hook
          (lambda ()
            (when (stringp (buffer-file-name))
              (let ((sh (getenv "SHELL")))
                (set (make-local-variable 'compile-command)
                     (concat
                      "cd " (projectile-project-root)
                      (cond
                       ((s-contains-p "bash" sh)
                        "_build_vs && cmake --build . -- -j4")
                       ((s-contains-p "fish" sh)
                        (format "_build ;and cmake --build . --target %s -- -j4"
                                (jco/cmake-project-name))))))))

            (jco/define-bindings c++-mode-map '(("<f6>" . compile)))

            (setq flycheck-clang-language-standard "c++14")

            (c-set-offset 'innamespace 0)
            (c-set-offset 'label '-)
            (fix-enum-class)

            (evil-leader/set-key "q i" 'jco/cpp-fix-constr-destr)
            (evil-leader/set-key "q d" 'jco/cpp-decl-to-def)
            (evil-leader/set-key "q D" 'jco/cpp-def-to-decl)
            (evil-leader/set-key "q f" 'jco/cpp-decl-field-accessors)
            (evil-leader/set-key "q r" 'jco/cpp-make-const-ref)
            (evil-leader/set-key "q t" 'jco/cpp-variadic-templatize)
            (evil-leader/set-key "q c" 'jco/cpp-fix-class-name)
            (evil-leader/set-key "q o" 'jco/cpp-override-method)

            (evil-leader/set-key "q C"
              (lambda ()
                (interactive)
                (jco/cpp-insert-class-name)
                (insert "::")))))

(provide 'init-cpp)

;;; init-cpp.el ends here
