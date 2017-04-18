;;; init-bootstrap.el --- Load all other configuration files.

;;; Commentary:
;;

;;; Code:

(let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path lisp-dir))

(defvar jco/init-base-files
  '(init-packages
    init-common-funs
    init-common
    ;; init-circe
    init-erc
    init-eshell
    init-fci
    init-font
    init-company
    init-ext-apps
    init-helm
    init-hydras
    init-id
    init-ido
    init-mu4e
    init-neotree
    init-projectile
    init-rotate
    init-slack
    init-tab
    init-utils
    init-wspace
    init-yas))

(defvar jco/init-prg-files
  '(init-common-programming
    init-bat
    init-flymake
    init-c
    init-cc
    init-clojure
    init-cmake
    init-cmake-ide
    init-conf
    init-cpp
    init-csharp
    init-ecb
    init-emacs-lisp
    init-haskell
    init-lang
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
    init-semantic
    init-standard-ml
    init-tex))

(defvar jco/init-last-files
  '(init-evil
    init-theme))

(defvar jco/init-files (append jco/init-base-files
                               jco/init-prg-files
                               jco/init-last-files))

(defvar jco/init-errors nil
  "If there are any initialization errors, they will be appended to this list.")

(defun jco/safe-require (feature)
  "Safely require FEATURE."
  (condition-case ex
      (require feature)
    ('error (add-to-list 'jco/init-errors
                         (format "[error loading \"%s\"]: %s"
                                 (symbol-name feature) ex)))))

(defun jco/safe-load-init-files ()
  "Safely requires all files in jco/init-files."
  (dolist (file jco/init-files)
    (jco/safe-require file)))

(defun jco/unsafe-load-init-files ()
  "Unsafely requires all files in jco/init-files."
  (dolist (file jco/init-files)
    (require file)))

(provide 'init-bootstrap)

(provide 'init-bootstrap)

;;; init-bootstrap.el ends here
