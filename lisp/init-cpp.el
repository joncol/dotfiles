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

(fset 'jco/cpp-decl-to-def
      [?_ ?: ?s ?/ ?Q ?_ ?I ?N ?V ?O ?K ?A ?B ?L ?E ? ?/ ?/ return ?f ?\( ?b
          ?\M-x ?j ?c ?o ?/ ?c ?p ?p ?- ?i ?n ?s ?e ?r ?t ?- ?c ?l ?a ?s ?s ?-
          ?n ?a ?m ?e return ?i ?: ?: ?k ?j ?/ ?\; return ?: ?s ?/ ? ?o ?v ?e ?r
          ?r ?i ?d ?e ?/ ?/ return ?/ ?\; return ?s return ?\{ return ?k ?j ?d
          ?d ?o ?k ?j ?j ?\[ ?\[ ?V ?\} ?= ?\} ?j ?\C-x ?\ ])

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

(fset 'jco/cpp-decl-field-accessors
   [?0 ?Y ?P ?P ?: ?s ?/ ?\\ ?s ?- ?* ?\\ ?\( ?. ?* ?\\ ?\) ? ?m ?_ ?\\ ?\( ?.
   ?* ?\\ ?\) ?\; ?/ ?v ?o ?i ?d ? ?s ?e ?t ?\\ ?, ?\( ?c ?a ?p ?i ?t ?a ?l ?i
   ?z ?e ? ?\\ ?2 ?\) ?\( ?\\ ?1 ? ?\\ ?2 end ?\; ?/ return ?= ?=
   ?j ?: ?s ?/ ?\\ ?s ?- ?* ?\\ ?\( ?. ?* ?\\ ?\) ? ?m ?_ ?\\ ?\( ?. ?* ?\\ ?\)
   ?\; ?/ ?\\ ?1 ? ?g ?e ?t ?\\ ?, ?\( ?c ?a ?p ?i ?t ?a ?l ?i ?z ?e ? ?\\ ?2
   end ?\( end ? ?c ?o ?n ?s ?t ?\; ?/ return ?= ?= ?k])

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
