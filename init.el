(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

(setq package-list '(yasnippet ack-and-a-half afternoon-theme ample-theme
                               angular-snippets airline-themes ag auto-complete
                               better-defaults cider cloc clojure-mode
                               clojure-snippets cmake-mode company company-cabal
                               company-ghc confluence csharp-mode csv-mode
                               cyberpunk-theme dash dirtree ecb edit-server epl
                               espresso-theme ethan-wspace evil evil-jumper
                               evil-leader evil-numbers evil-matchit
                               evil-nerd-commenter evil-paredit
                               evil-search-highlight-persist evil-surround
                               exec-path-from-shell fill-column-indicator
                               flatland-theme flatui-theme flx-ido flycheck
                               fsharp-mode ggtags ghci-completion glsl-mode
                               go-snippets goto-chg goto-last-change
                               grandshell-theme graphviz-dot-mode
                               gruber-darker-theme gruvbox-theme haskell-mode
                               helm helm-ag helm-company helm-gtags helm-swoop
                               hemisu-theme htmlize java-snippets jira
                               leuven-theme lua-mode magit markdown-mode
                               molokai-theme monky monokai-theme neotree
                               omnisharp org-plus-contrib org-present ox-reveal
                               paredit pkg-info plantuml-mode popup pos-tip
                               powerline project-explorer projectile qml-mode
                               racket-mode rvm rainbow-delimiters rainbow-mode
                               robe rspec-mode rubocop ruby-end rust-mode slime
                               sml-mode solarized-theme soothe-theme toml-mode
                               undo-tree xml-rpc yaml-mode))

(add-to-list 'load-path "~/repos/ghc-mod/elisp")
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(unless (package-installed-p 'yasnippet)
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'semantic)
(require 'semantic/bovine/gcc)


(global-set-key (kbd "<f12>") '(lambda ()
                                 (interactive)
                                 (message "Current major mode: %s" major-mode)))
;;; Set name and email
(require 's)
(setq user-full-name "Jonas Collberg")
(setq user-mail-address
      (concat (s-replace " " "." (downcase user-full-name)) "@"
              (if (eq t (compare-strings (system-name) nil nil
                                         "orz-lap01" nil nil t))
                  "orzone.com"
                "gmail.com")))



(modify-syntax-entry ?_ "w") ;; do not treat _ as word separator
(edit-server-start)
(global-auto-revert-mode t)
(setq ring-bell-function 'ignore)
(global-font-lock-mode 1)
(show-paren-mode 1)
(tool-bar-mode -1)
(global-set-key (kbd "C-c t f") 'toggle-frame-fullscreen)
(electric-pair-mode 1)
(global-linum-mode t)
(setq-default tab-width 4)
(when (display-graphic-p) (global-hl-line-mode 1))
(ido-mode)
(flx-ido-mode)
;; (setq ido-enable-flex-matching t)
(setq projectile-completion-system 'ido)
(require 'jira)
(setq jira-url "http://jira.combination.se:8080/rpc/xmlrpc")
(setq scroll-step           1
      scroll-conservatively 10000)
(setq inhibit-startup-message t)
(setq yas-snippet-dirs '("~/.emacs.d/snippets" yas-installed-snippets-dir))
(yas-global-mode 1)
(setq yas-indent-line 'none)
(setq safe-local-variable-values (quote ((require-final-newline) require-final-newline)))
(load-library "iso-transl")

(global-set-key (kbd "<f4>")
                (lambda ()
                  (interactive)
                  (start-process "gvim" nil
                                 "gvim"
                                 (format "+%d" (line-number-at-pos))
                                 (buffer-file-name))))

(global-set-key (kbd "C-x a r") 'align-regexp)
;; align with spaces only
(defadvice align-regexp (around align-regexp-with-spaces activate compile)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))

(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)
(rainbow-mode 1)
(setq system-time-locale "C")
(setq display-time-string-forms '(24-hours ":" minutes))
(display-time-mode 1)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq ecb-tip-of-the-day nil)

(global-set-key (kbd "C-c C-b") 'help-go-back)

(global-set-key (kbd "<f9>") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "S-<f9>") (lambda () (interactive) (load-file "~/.emacs.d/init.el")))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(defun date (arg)
  (insert (if arg
              (format-time-string "%d.%m.%Y")
            (format-time-string "%Y-%m-%d"))))
(defun timestamp (arg)
  (insert (if arg
              (format-time-string "%Y-%m-%dT%H:%M:%S")
            (format-time-string "%H:%M:%S"))))

(global-set-key (kbd "C-c i d") (lambda () (interactive) (date nil)))
(global-set-key (kbd "C-c i T") (lambda () (interactive) (timestamp t)))
(global-set-key (kbd "C-c i t") (lambda () (interactive) (timestamp nil)))

(setq cider-show-error-buffer 'nil)

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete (use-ghc-complete)
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (progn
              (company-complete-common)
              (when use-ghc-complete
                (ghc-complete)))
          (indent-for-tab-command)))))

(global-set-key [tab] (lambda () (interactive) (tab-indent-or-complete nil)))

(defun rotate-word-at-point ()
  "Rotate word at point based on sets in `rotate-text-rotations'."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word))
        (opoint (point)))
    (when (consp bounds)
      (let ((beg (car bounds))
            (end (copy-marker (cdr bounds))))
        (rotate-region beg end)
        (goto-char (if (> opoint end) end opoint))))))

;;; helm setup

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(setq helm-gtags-ignore-case t
      helm-gtags-auto-update t
      helm-gtags-use-input-at-cursor t
      helm-gtags-pulse-at-cursor t
      ;; helm-gtags-prefix-key "\C-cg"
      helm-gtags-suggested-key-mapping t
      helm-ag-base-command "ag --nocolor --nogroup --line-numbers --smart-case --ignore #*#;TAGS;*.html;*.json;*.map;*.opensdf;*.pdf;*.sdf"
      helm-ag-insert-at-point 'word)

(when (not (eq system-type 'windows-nt))
  (setq helm-ag-ignore-patterns
        '("#*#" "TAGS" "*.html" "*.json" "*.map" "*.opensdf" "*.pdf" "*.sdf")))

;; enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(helm-mode 1)


;;; Font

(cond
 ((and (eq system-type 'windows-nt) (display-graphic-p))
  (set-frame-font "Inconsolata-12")
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 100 60))

 ((and (eq system-type 'gnu/linux) (display-graphic-p))
  (set-frame-font "Bitstream Vera Sans Mono")
  (set-face-attribute 'default nil :height 105)
  (set-frame-size (selected-frame) 93 64))

 ((eq system-type 'darwin)
  (setq mac-right-option-modifier 'none)
  (when (display-graphic-p) (set-frame-size (selected-frame) 93 60)))
 )

(when (not (eq system-type 'windows-nt))
    (setq projectile-indexing-method 'native))

(if (display-graphic-p)
    (load-theme 'molokai t)

  (load-theme 'molokai t))

(require 'fill-column-indicator)
(setq-default fill-column 80)
(setq fci-rule-width 1)
(setq fci-rule-color "#ff0000")


;;; Fix for htmlize producing garbage newlines when using fci-mode.

(defvar modi/htmlize-initial-fci-state nil
  "Variable to store the state of `fci-mode' when `htmlize-buffer' is called.")
(defvar modi/htmlize-initial-flyspell-state nil
  "Variable to store the state of `flyspell-mode' when `htmlize-buffer' is called.")

(defun modi/htmlize-before-hook-fn ()
  (when (fboundp 'fci-mode)
    (setq modi/htmlize-initial-fci-state fci-mode)
    (when fci-mode
      (fci-mode -1)))
  (when (fboundp 'flyspell-mode)
    (setq modi/htmlize-initial-flyspell-state flyspell-mode)
    (when flyspell-mode
      (flyspell-mode -1))))
(add-hook 'htmlize-before-hook #'modi/htmlize-before-hook-fn)

(defun modi/htmlize-after-hook-fn ()
  (when (fboundp 'fci-mode)
    (when modi/htmlize-initial-fci-state
      (fci-mode 1)))
  (when (fboundp 'flyspell-mode)
    (when modi/htmlize-initial-flyspell-state
      (flyspell-mode 1))))
(add-hook 'htmlize-after-hook #'modi/htmlize-after-hook-fn)

;;; Neo Tree stuff
(global-set-key [f2] 'neotree-toggle)
(setq neo-show-header nil)

(add-hook 'neotree-mode-hook 'neotree-mode-hook t)

;;; autocomplete
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)
(auto-complete-mode)
(setq ac-ignore-case 'smart)
(setq company-dabbrev-ignore-case 'keep-prefix)
(setq company-dabbrev-code-ignore-case nil)
(setq company-dabbrev-downcase nil)

;;; Racket mode
(setq racket-program "/usr/local/bin/racket")
(setq raco-program "/usr/local/bin/raco")

;;; Org-mode settings
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)
(setq org-reveal-hlevel 2)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)
   (dot . t)
   (plantuml . t)))

(setq org-confirm-babel-evaluate nil)
(if (eq system-type 'windows-nt)
    (setq org-ditaa-jar-path "c:/tools/misc/ditaa.jar"
          org-plantuml-jar-path "c:/tools/misc/plantuml.jar")
  (setq org-ditaa-jar-path "/usr/local/bin/ditaa.jar"
        org-plantuml-jar-path "/usr/local/bin/plantuml.jar"))

;;; whitespace / tabs
(electric-indent-mode 1)
(setq-default indent-tabs-mode nil)

(defun delete-trailing-whitespace-then-newline ()
  (interactive)
  (delete-trailing-whitespace (line-beginning-position) (line-end-position))
  (newline-and-indent))

(global-set-key (kbd "RET") 'delete-trailing-whitespace-then-newline)

;;; ethan-wspace
(defun no-final-newline ()
  (setq require-final-newline nil)
  (setq mode-require-final-newline nil))

(no-final-newline)

(require 'ethan-wspace)

(global-ethan-wspace-mode 1)

(defun tabs-are-ok ()
  (setq ethan-wspace-errors (remove 'tabs ethan-wspace-errors)))
(add-hook 'makefile-mode-hook 'tabs-are-ok)

(add-hook 'sml-mode-hook 'no-final-newline t)
(add-hook 'fsharp-mode-hook 'no-final-newline t)
(add-hook 'ruby-mode-hook 'no-final-newline t)

(defadvice ruby-mode-variables (after reset-final-newline activate compile)
  "Reset final-newline that ruby-mode enforces but conflicts with ethan-wspace."
  (setq require-final-newline nil)
  (setq mode-require-final-newline nil))

(global-whitespace-mode 1)
(setq-default whitespace-style '(face tabs trailing
                                      space-before-tab indentation
                                      empty space-after-tab tab-mark))
(set-face-background 'whitespace-trailing "#ff0000")

;;; recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(require 'omnisharp)
(setq omnisharp-company-do-template-completion t)

;;; Confluence

(setq confluence-url "http://wiki.combination.se/rpc/xmlrpc")
(setq confluence-default-space-alist (list (cons confluence-url "Company")
                                           (cons confluence-url "Alchemy")
                                           (cons confluence-url "Development")
                                           (cons confluence-url "Testing")
                                           (cons confluence-url "Operations")
                                           (cons confluence-url "Projects")))
(setq confluence-save-credentials t)

(autoload 'confluence-get-page "confluence" nil t)

(eval-after-load "confluence"
  '(progn
     (require 'longlines)
     (progn
       (add-hook 'confluence-mode-hook 'longlines-mode)
       (add-hook 'confluence-before-save-hook 'longlines-before-revert-hook)
       (add-hook 'confluence-before-revert-hook 'longlines-before-revert-hook)
       (add-hook 'confluence-mode-hook
                 '(lambda ()
                    (local-set-key "\C-j" 'confluence-newline-and-indent))))))

;; LongLines mode: http://www.emacswiki.org/emacs-en/LongLines
(autoload 'longlines-mode "longlines" "LongLines Mode." t)

(eval-after-load "longlines"
  '(progn
     (defvar longlines-mode-was-active nil)
     (make-variable-buffer-local 'longlines-mode-was-active)

     (defun longlines-suspend ()
       (if longlines-mode
           (progn
             (setq longlines-mode-was-active t)
             (longlines-mode 0))))

     (defun longlines-restore ()
       (if longlines-mode-was-active
           (progn
             (setq longlines-mode-was-active nil)
             (longlines-mode 1))))

     ;; longlines doesn't play well with ediff, so suspend it during diffs
     (defadvice ediff-make-temp-file (before make-temp-file-suspend-ll
                                             activate compile preactivate)
       "Suspend longlines when running ediff."
       (with-current-buffer (ad-get-arg 0)
         (longlines-suspend)))

     (add-hook 'ediff-cleanup-hook
               '(lambda ()
                  (dolist (tmp-buf (list ediff-buffer-A
                                         ediff-buffer-B
                                         ediff-buffer-C))
                    (if (buffer-live-p tmp-buf)
                        (with-current-buffer tmp-buf
                          (longlines-restore))))))))

;; keybindings (change to suit)

;; open confluence page
(global-set-key "\C-xwf" 'confluence-get-page)
(global-set-key "\C-xwb" 'confluence-browse-page)

;; setup confluence mode
(add-hook 'confluence-mode-hook
          '(lambda ()
             (local-set-key "\C-xw" confluence-prefix-map)))

;; (global-set-key "\t" 'company-complete-common)

; (make-variable-buffer-local 'line-move-visual)
; (defadvice previous-line (around avoid-jumpy-fci activate)
;   (if (and (symbol-value 'fci-mode) (> (count-lines 1 (point)) 0))
;       (prog (fci-mode -1) ad-do-it (fci-mode 1))
;     ad-do-it))


;; rotate text

(defvar rotate-text-rotations
  '(("true" "false")
    ("True" "False")
    ("yes" "no")
    ("YES" "NO"))
  "List of text rotation sets.")

(defun rotate-region (beg end)
  "Rotate all matches in `rotate-text-rotations' between point and mark."
  (interactive "r")
  (let ((regexp (rotate-convert-rotations-to-regexp
                 rotate-text-rotations))
        (end-mark (copy-marker end)))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward regexp (marker-position end-mark) t)
        (let* ((found (match-string 0))
               (replace (rotate-next found)))
          (replace-match replace))))))

(defun rotate-string (string &optional rotations)
  "Rotate all matches in STRING using associations in ROTATIONS.
If ROTATIONS are not given it defaults to `rotate-text-rotations'."
  (let ((regexp (rotate-convert-rotations-to-regexp
                 (or rotations rotate-text-rotations)))
        (start 0))
    (while (string-match regexp string start)
      (let* ((found (match-string 0 string))
             (replace (rotate-next
                       found
                       (or rotations rotate-text-rotations))))
        (setq start (+ (match-end 0)
                       (- (length replace) (length found))))
        (setq string (replace-match replace nil t string))))
    string))

(defun rotate-next (string &optional rotations)
  "Return the next element after STRING in ROTATIONS."
  (let ((rots (rotate-get-rotations-for
               string
               (or rotations rotate-text-rotations))))
    (if (> (length rots) 1)
        (error (format "Ambiguous rotation for %s" string))
      (if (< (length rots) 1)
          ;; If we get this far, this should not occur:
          (error (format "Unknown rotation for %s" string))
        (let ((occurs-in-rots (member string (car rots))))
          (if (null occurs-in-rots)
              ;; If we get this far, this should *never* occur:
              (error (format "Unknown rotation for %s" string))
            (if (null (cdr occurs-in-rots))
                (caar rots)
              (cadr occurs-in-rots))))))))

(defun rotate-get-rotations-for (string &optional rotations)
  "Return the string rotations for STRING in ROTATIONS."
  (remq nil (mapcar (lambda (rot) (if (member string rot) rot))
                    (or rotations rotate-text-rotations))))

(defun rotate-convert-rotations-to-regexp (rotations)
  (regexp-opt (rotate-flatten-list rotations)))

(defun rotate-flatten-list (list-of-lists)
  "Flatten LIST-OF-LISTS to a single list.
Example:
  (rotate-flatten-list '((a b c) (1 ((2 3)))))
    => (a b c 1 2 3)"
  (if (null list-of-lists)
      list-of-lists
    (if (listp list-of-lists)
        (append (rotate-flatten-list (car list-of-lists))
                (rotate-flatten-list (cdr list-of-lists)))
      (list list-of-lists))))

(defun indent-or-rotate ()
  "If point is at end of a word, then else indent the line."
  (interactive)
  (if (looking-at "\\>")
      (rotate-region (save-excursion (forward-word -1) (point))
                     (point))
    (indent-for-tab-command)))

;;; mode hooks

(defun common-prog ()
  (rainbow-delimiters-mode 1)
  (fci-mode 1)
  (local-set-key (kbd "C-C p s a") 'helm-ag-project-root)
  (modify-syntax-entry ?_ "w") ;; do not treat _ as word separator
  )

(add-hook 'markdown-mode-hook 'my-markdown-mode-hook t)
(defun my-markdown-mode-hook ()
  (common-prog)
  (projectile-mode 1)
  (auto-fill-mode)
  )

(add-hook 'clojure-mode-hook 'my-clojure-mode-hook t)
(defun my-clojure-mode-hook ()
  (common-prog)
  (projectile-mode 1)
  )

(add-hook 'lua-mode-hook 'my-lua-mode-hook t)
(defun my-lua-mode-hook ()
  (common-prog)
  (projectile-mode 1)
  (setq lua-indent-level 4)
  )

(add-hook 'org-mode-hook 'my-org-mode-hook t)
(defun my-org-mode-hook ()
  (common-prog)
  (setq org-src-fontify-natively t)
  (load-library "ox-reveal")
  (auto-fill-mode)

;;; Embedding youtube links in org-mode

  (defvar yt-iframe-format
    (concat "<iframe width=\"560\""
            " height=\"315\""
            " src=\"https://www.youtube.com/embed/%s?rel=0\""
            " frameborder=\"0\""
            " allowfullscreen>%s</iframe>"))

 (defvar ytnc-iframe-format
    (concat "<iframe width=\"560\""
            " height=\"315\""
            " src=\"https://www.youtube.com/embed/%s?rel=0&controls=0\""
            " frameborder=\"0\""
            " allowfullscreen>%s</iframe>"))

 (org-add-link-type
  "yt"
  (lambda (handle)
    (browse-url
     (concat "https://www.youtube.com/embed/"
             handle)))
  (lambda (path desc backend)
    (cl-case backend
      (html (format yt-iframe-format
                    path (or desc "")))
      (latex (format "\href{%s}{%s}"
                     path (or desc "video"))))))

 (org-add-link-type
  "ytnc"
  (lambda (handle)
    (browse-url
     (concat "https://www.youtube.com/embed/"
             handle)))
  (lambda (path desc backend)
    (cl-case backend
      (html (format ytnc-iframe-format
                    path (or desc "")))
      (latex (format "\href{%s}{%s}"
                     path (or desc "video"))))))
 )

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'global-company-mode-hook 'my-global-company-mode-hook t)
(defun my-global-company-mode-hook ()
  ; bigger popup window
  (setq company-tooltip-limit 20)

  ; decrease delay before autocompletion popup shows
  (setq company-idle-delay .3)

  ; remove blinking
  (setq company-echo-delay 0)

  ; start autocompletion only after typing
  (setq company-begin-commands '(self-insert-command))

  (define-key company-active-map (kbd "j") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "k") 'company-select-previous-or-abort))

(defun all-lisp-modes ()
  (common-prog)
  (paredit-mode)
  (evil-paredit-mode))

(add-hook 'lisp-mode-hook 'my-lisp-mode-hook t)
(defun my-lisp-mode-hook ()
  (all-lisp-modes)
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl")
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (global-set-key "\M-." 'slime-edit-definition))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook t)
(defun my-emacs-lisp-mode-hook ()
  (all-lisp-modes))

(add-hook 'clojure-mode-hook 'my-clojure-mode-hook t)
(defun my-clojure-mode-hook ()
  (all-lisp-modes))

(add-hook 'scheme-mode-hook 'my-scheme-mode-hook t)
(defun my-scheme-mode-hook ()
  (all-lisp-modes)
  (setq scheme-mit-dialect nil))

;;; Python

(add-hook 'python-mode-hook 'my-python-mode-hook t)
(defun my-python-mode-hook ()
  (common-prog)
  (projectile-mode 1))

;;; Racket

(add-hook 'racket-mode-hook 'my-racket-mode-hook t)
(defun my-racket-mode-hook()
  (common-prog))

(eval-after-load 'racket-mode
  '(progn
     (define-key racket-mode-map (kbd "C-c C-l") 'racket-run)))

;;; C

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook t)
(defun my-c-mode-common-hook ()
  (common-prog)
  (setq-default backward-delete-function nil)
  (c-add-style "my-c-style"
               '((c-basic-offset . 4)
                 (c-offsets-alist (label . +))
                 ))
  (c-set-style "my-c-style")
  (c-set-offset 'substatement-open '0)
  (c-set-offset 'inline-open '0)
  ;; (c-set-offset 'block-open '+)
  ;; (c-set-offset 'brace-list-open '+)
  ;; (c-set-offset 'case-label '+)
  (setq tab-width 4)
  ;; (setq indent-tabs-mode t)
  (setq align-to-tab-stop nil)
  (c-set-offset 'substatement-open 0)
  (company-mode)
  (local-set-key (kbd "<tab>") 'company-complete-common)
  ;; (yas-minor-mode 1)
  (rainbow-delimiters-mode 1)
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (global-set-key "\M-." 'ggtags-find-tag-dwim)
  (local-set-key  (kbd "C-x o") 'ff-find-other-file)
  (local-set-key  (kbd "C-x C-o") 'ff-find-other-file)
  (projectile-mode 1)
  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
    (ggtags-mode 1)
    (define-key ggtags-mode-map (kbd "C-c t s") 'ggtags-find-other-symbol)
    (define-key ggtags-mode-map (kbd "C-c t h") 'ggtags-view-tag-history)
    (define-key ggtags-mode-map (kbd "C-c t r") 'ggtags-find-reference)
    (define-key ggtags-mode-map (kbd "C-c t c") 'ggtags-create-tags)
    (define-key ggtags-mode-map (kbd "C-c t u") 'ggtags-update-tags)

    (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark))

  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
  (semantic-mode 1)
  (global-ede-mode t)
  (ede-enable-generic-projects)
  (evil-leader/set-key "a" 'projectile-find-other-file)
  )

;;; C++

(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-hook 'c++-mode-hook 'my-c++-mode-hook t)
(defun my-c++-mode-hook ()
  (setq compile-command (concat "cd " (projectile-project-root) "debug ;and make -j4 ;and ctest"))
  (global-set-key (kbd "<f6>") 'compile)
  (c-set-offset 'innamespace '0)

  (defadvice c-lineup-arglist (around my activate)
    "Improve indentation of continued C++11 lambda function opened as argument."
    (setq ad-return-value
          (if (and (equal major-mode 'c++-mode)
                   (ignore-errors
                     (save-excursion
                       (goto-char (c-langelem-pos langelem))
                       ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                       ;;   and with unclosed brace.
                       ;; (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
                       (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
              0                           ; no additional indent
            ad-do-it)))                   ; default behavior
  )

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
  (add-to-list 'c-offsets-alist '(statement-cont . align-enum-class-closing-brace)))

(add-hook 'c++-mode-hook 'fix-enum-class)

;;; C#

(add-hook 'csharp-mode-hook 'my-csharp-mode-hook t)
(defun my-csharp-mode-hook ()
  (common-prog)
  (electric-pair-mode 0)
  (add-to-list 'company-backends 'company-omnisharp)
  (c-set-style "c#")
  (omnisharp-mode)
  (flycheck-mode)
  (local-set-key "\M-g" 'omnisharp-go-to-definition)
  )

;;; F#

(add-hook 'fsharp-mode-hook 'my-fsharp-mode-hook t)
(defun my-fsharp-mode-hook ()
  (common-prog)
  (omnisharp-mode)
  )

;;; Ruby

(add-hook 'ruby-mode-hook 'my-ruby-mode-hook t)
(defun my-ruby-mode-hook ()
  (common-prog)
  (setq evil-shift-width 2)
  (rvm-use-default)
  (global-set-key (kbd "C-c r a") 'rvm-activate-corresponding-ruby)
  (add-to-list 'company-backends 'company-robe)
  (ruby-end-mode 1)
  (projectile-mode 1))

(defun newline-and-indent-relative ()
  (interactive)
  (newline)
  (indent-to-column (save-excursion
                      (forward-line -1)
                      (back-to-indentation)
                      (current-column))))

;;; Haskell

(add-hook 'haskell-mode-hook 'my-haskell-mode-hook t)
(defun my-haskell-mode-hook ()
  (common-prog)
  (turn-on-haskell-doc-mode)
  (remove-hook 'haskell-mode-hook 'turn-on-haskell-indent)

  ;; just use tab-stop indentation
  ;; (turn-on-haskell-simple-indent)
  ;; (setq indent-line-function 'tab-to-tab-stop)

  (turn-on-haskell-indentation)
  (haskell-indentation-disable-show-indentations)

  (setq tab-stop-list
        (loop for i from 0 upto 120 by 2 collect i))

  (define-key global-map (kbd "RET") 'newline-and-indent-relative)
  (global-unset-key [tab])
  (local-set-key [tab] (lambda () (interactive) (tab-indent-or-complete 1)))

  ;; (structured-haskell-mode)
  ;; (set-face-background 'shm-current-face "#05303b")
  ;; (set-face-background 'shm-quarantine-face "#05303b")

  (setq evil-shift-width 2)

  (company-mode)
  (add-to-list 'company-backends 'company-cabal)
  (add-to-list 'company-backends 'company-ghc)
  (setq company-ghc-show-info t)
  (projectile-mode 1)
  (setq haskell-interactive-popup-errors nil)

  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key haskell-indentation-mode-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-normal-state-map (kbd "M-.") nil)

  (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
    (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
    (add-to-list 'exec-path my-cabal-path))

  (setq haskell-process-auto-import-loaded-modules t)
  (setq haskell-process-log t)
  (setq haskell-process-suggest-remove-import-lines t)
  (setq haskell-process-type 'cabal-repl)

  (setq haskell-tags-on-save t)
)

(eval-after-load 'haskell-mode
  '(progn
     (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
     (define-key haskell-mode-map (kbd "S-<f8>") 'haskell-sort-imports)))

(eval-after-load 'haskell-mode
  '(progn
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
     (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
     (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))

(eval-after-load 'haskell-cabal
  '(progn
     (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
     (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
     (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile))
(eval-after-load 'haskell-cabal
  '(define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile))

;;; Standard ML

(add-hook 'sml-mode-hook 'my-sml-mode-hook t)
(defun my-sml-mode-hook ()
  (common-prog)
  (setq sml-indent-level 2)
  (setq evil-shift-width 2)
  (setq sml-program-name "/usr/local/bin/sml"))

(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))
(add-hook 'qml-mode-hook 'my-qml-mode-hook t)
(defun my-qml-mode-hook ()
  (common-prog)
  (projectile-mode 1)
  )

(add-hook 'nxml-mode-hook 'my-xml-mode-hook t)
(defun my-xml-mode-hook ()
  (common-prog))

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'markdown-mode-hook 'my-markdown-mode-hook t)
(defun my-markdown-mode-hook ()
  (common-prog)
  (setq evil-shift-width 4)
  )

(add-hook 'latex-mode-hook 'my-latex-mode-hook t)
(defun my-latex-mode-hook ()
  (common-prog)
  (setq evil-shift-width 2)
  )

(add-hook 'yaml-mode-hook 'my-yaml-mode-hook t)
(defun my-yaml-mode-hook ()
  (common-prog)
  (setq evil-shift-width 2)
  )

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))

(add-hook 'cmake-mode-hook 'my-cmake-mode-hook t)
(defun my-cmake-mode-hook ()
  (common-prog)
  (projectile-mode 1)
  )

(add-hook 'tex-mode-hook 'my-tex-mode-hook t)
(defun my-tex-mode-hook ()
  (common-prog)
  (setq evil-shift-width 2)
  )

(add-hook 'rust-mode-hook 'my-rust-mode-hook t)
(defun my-rust-mode-hook ()
  (common-prog))

(add-hook 'toml-mode-hook 'my-toml-mode-hook t)
(defun my-toml-mode-hook ()
  (common-prog))

;; (defun my-compilation-mode-hook ()
;;   (when (not (get-buffer-window "*compilation*"))
;;     (save-selected-window
;;       (save-excursion
;;         (let* ((w (split-window-vertically))
;;                (h (window-height w)))
;;           (select-window w)
;;           (switch-to-buffer "*compilation*")
;;           (shrink-window (- h 10)))))))

;; (add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

;; airline is too slow on Mac OS X
(if (not (eq system-type 'darwin))
    (load-theme 'airline-powerlineish t))

(global-evil-leader-mode 1)
(evil-leader/set-leader ",")
(evil-mode 1)
(evil-jumper-mode 1)
(global-evil-search-highlight-persist t)

(eval-after-load "evil"
            ;; disable evil-mode for the following modes
            (dolist (mode-map '((ag-mode . emacs)
                                (comint-mode . emacs)
                                (diff-mode . emacs)
                                (eshell-mode . emacs)
                                (eww-mode . emacs)
                                ;; (fundamental-mode . emacs)
                                (git-commit-mode . insert)
                                (git-rebase-mode . emacs)
                                (paradox-menu-mode . emacs)
                                (term-mode . emacs)))
              (evil-set-initial-state `,(car mode-map) `,(cdr mode-map))))

(global-evil-matchit-mode 1)
(global-evil-surround-mode 1)
(setq evil-normal-state-cursor '("green" box))
(setq evil-insert-state-cursor '("green" bar))

;; (setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
;; (setq evil-emacs-state-modes nil)

(defun my-move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location. "
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))

(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")

(defun neotree-mode-hook ()
  (hl-line-mode 1)
  (define-key evil-normal-state-map "\C-u" 'neotree-toggle)
  (define-key evil-insert-state-map "\C-u" 'neotree-toggle)
  (define-key evil-visual-state-map "\C-u" 'neotree-toggle)

  (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
  (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
  (define-key evil-normal-state-local-map (kbd "c") 'neotree-change-root)
  (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
  (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
  (define-key evil-normal-state-local-map (kbd "v") 'neotree-enter-vertical-split)
  (define-key evil-normal-state-local-map (kbd "s") 'neotree-enter-horizontal-split)
  (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
)

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

(add-hook 'snippet-mode-hook
          '(lambda ()
             (ethan-wspace-mode -1))
          t)

;;; Evil mode stuff

; make "kj" exit out of insert mode
(define-key evil-insert-state-map "k" #'cofi/maybe-exit)
(evil-define-command cofi/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "k")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?j)
                           nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?j))
        (delete-char -1)
        (set-buffer-modified-p modified)
        (push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
                                              (list evt))))))))

(evil-define-motion evil-ruby-jump-item (count)
  :jump t
  :type inclusive
  (cond ((or (string-match ruby-block-beg-re (current-word))
             (string-match "describe" (current-word))
             (string-match "context" (current-word))
             (string-match "it" (current-word)))
         (ruby-end-of-block count))
        ((string-match ruby-block-end-re (current-word))
         (ruby-beginning-of-block count))
        (t
         (evil-jump-item count))))

(define-key evil-normal-state-map "+" 'rotate-word-at-point)
(define-key evil-normal-state-map (kbd "C-w C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-w C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-w C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-w C-l") 'evil-window-right)
(setq evil-flash-delay 3600)

(defun run-on-current-buffer (program &rest args)
  (let ((all-args (-snoc args (buffer-file-name))))
    (apply (-partial 'start-process program nil program) all-args)))

(evil-leader/set-key "t a"
  (lambda ()
    (interactive)
    (run-on-current-buffer "thg" "annotate")))

(evil-leader/set-key "t l"
  (lambda ()
    (interactive)
    (run-on-current-buffer "thg" "log")))

(evil-leader/set-key "t d"
  (lambda ()
    (interactive)
    (if (eq system-type 'windows-nt)
        (run-on-current-buffer "thg" "vdiff")
      (run-on-current-buffer "hg" "opendiff"))))

(evil-leader/set-key "t s"
  (lambda ()
    (interactive)
    (run-on-current-buffer "thg" "shelve")))

(evil-leader/set-key
  "d" 'vc-diff
  "m a" 'monky-blame-current-file
  "h s" 'monky-status
  "g s" 'magit-status)

(global-set-key (kbd "C-x C-SPC") 'evil-search-highlight-persist-remove-all)

;;; evil-nerd-commenter

(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)

(evil-leader/set-key
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cv" 'evilnc-toggle-invert-comment-line-by-line
)

;;; helm bindings

(global-set-key (kbd "C-x b") 'helm-buffers-list)
(evil-leader/set-key "b" 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-r") 'helm-recentf)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

(evil-leader/set-key "r" 'helm-resume)

;;; helm-swoop

(require 'helm-swoop)
(global-set-key (kbd "M-i") 'helm-swoop)
(evil-leader/set-key "s" 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-M-i") 'helm-multi-swoop)
(evil-leader/set-key "m s" 'helm-multi-swoop)
(global-set-key (kbd "C-M-S-i") 'helm-multi-swoop-all)
(evil-leader/set-key "m S" 'helm-multi-swoop-all)

(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
(define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)
(define-key helm-swoop-map
  (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

(setq helm-multi-swoop-edit-save t)
(setq helm-swoop-split-with-multiple-windows nil)
(setq helm-swoop-split-direction 'split-window-vertically)
(setq helm-swoop-speed-or-color t)
(setq helm-swoop-move-to-line-cycle t)
(setq helm-swoop-use-line-number-face t)

(setq helm-multi-swoop-ignore-buffers-match
      (concat helm-multi-swoop-ignore-buffers-match "\\|TAGS"))

(custom-set-variables
 '(ecb-options-version "2.40"))
