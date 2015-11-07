(let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path lisp-dir))

(defvar init-files
  '(init-packages
    init-custom-funs
    init-common
    init-helm
    init-ido
    init-neotree
    init-rotate
    init-yas
    init-common-programming
    init-flymake
    init-global-company-mode
    init-c-common
    init-clojure
    init-cmake
    init-cpp
    init-csharp
    init-emacs-lisp
    init-haskell
    init-old-config
    init-keybindings
    init-latex
    init-lisp
    init-lua
    init-markdown
    init-org
    init-qml
    init-racket
    init-ruby
    init-scheme
    init-standard-ml
    init-tex
    init-yaml
    init-evil))

(defun jco/safe-require (feature)
  "Safely require FEATURE"
  (condition-case ex
      (require feature)
    ('error (add-to-list 'jco/init-errors
                         (format "[error loading \"%s\"]: %s"
                                 (symbol-name feature) ex)))))

(defun jco/safe-load-init-files ()
  (dolist (file init-files)
    (jco/safe-require file)))

(defun jco/unsafe-load-init-files ()
  (dolist (file init-files)
    (require file)))

(provide 'init-bootstrap)
