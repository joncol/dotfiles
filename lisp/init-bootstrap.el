(let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path lisp-dir))

(defvar jco/init-base-files
  '(init-packages
    init-common-funs
    init-common
    init-circe
    init-erc
    init-eshell
    init-fci
    init-font
    init-company-mode
    init-ext-apps
    init-helm-and-projectile
    init-hydras
    init-id
    init-ido
    init-message
    init-mu4e
    init-neotree
    init-rotate
    init-utils
    init-wspace
    init-yas))

(defvar jco/init-prg-files
  '(init-common-programming
    init-flymake
    init-bat
    init-c
    init-cc
    init-clojure
    init-cmake
    init-conf
    init-cpp
    init-csharp
    init-ecb
    init-emacs-lisp
    init-haskell
    init-latex
    init-lisp
    init-lua
    init-macros
    init-markdown
    init-nsis
    init-nxml
    init-org
    init-python
    init-qml
    init-racket
    init-rfc
    init-ruby
    init-scheme
    init-standard-ml
    init-tex
    init-yaml
    init-evil
    init-theme))

(defvar jco/init-files (append jco/init-base-files jco/init-prg-files))

(defun jco/safe-require (feature)
  "Safely require FEATURE"
  (condition-case ex
      (require feature)
    ('error (add-to-list 'jco/init-errors
                         (format "[error loading \"%s\"]: %s"
                                 (symbol-name feature) ex)))))

(defun jco/safe-load-init-files ()
  (dolist (file jco/init-files)
    (jco/safe-require file)))

(defun jco/unsafe-load-init-files ()
  (dolist (file jco/init-files)
    (require file)))

(provide 'init-bootstrap)
