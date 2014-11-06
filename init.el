(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

(setq package-list '(yasnippet angular-snippets clojure-snippets color-theme color-theme-solarized company confluence ethan-wspace evil evil-matchit evil-surround fill-column-indicator fsharp-mode ghc go-snippets goto-chg goto-last-change haskell-mode hi2 java-snippets jira neotree omnisharp csharp-mode flycheck auto-complete dash org pkg-info epl popup pos-tip racket-mode rainbow-delimiters sml-mode undo-tree xml-rpc))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(setq ring-bell-function 'ignore)
(global-font-lock-mode 1)
(show-paren-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(global-set-key (kbd "\C-c\ \C-f") 'toggle-frame-fullscreen)
(electric-pair-mode 1)
(global-linum-mode t)
(setq-default tab-width 4)
(global-set-key [f2] 'neotree-toggle)
(ido-mode)
(require 'jira)
(setq jira-url "http://jira.combination.se:8080/rpc/xmlrpc")
(setq scroll-step           1
      scroll-conservatively 10000)
(setq inhibit-startup-message t)
(setq yas-snippet-dirs '("~/.emacs.d/snippets" yas-installed-snippets-dir))
(yas-global-mode 1)
(setq safe-local-variable-values (quote ((require-final-newline) require-final-newline)))

;;; color theme
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)

(if (eq system-type 'windows-nt)
    (progn
      (custom-set-faces
       '(default ((t (:family "Inconsolata" :foundry "outline" :slant normal
       :weight normal :height 120 :width normal)))))
      (set-frame-position (selected-frame) 1920 0)
      (set-frame-size (selected-frame) 180 60)
      (color-theme-solarized 'dark))
  (progn
    (when (display-graphic-p) (set-frame-size (selected-frame) 180 80))
    (color-theme-solarized 'light)))

(require 'fill-column-indicator)
(setq-default fill-column 80)
(setq fci-rule-width 1)
(setq fci-rule-color "#ff0000")
;; (add-hook 'after-change-major-mode-hook 'fci-mode)

(evil-mode 1)
(global-evil-matchit-mode 1)
(global-evil-surround-mode 1)

;;; autocomplete
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)
(auto-complete-mode)

(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'none))

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

;;; whitespace / tabs
(electric-indent-mode 1)
(setq-default indent-tabs-mode nil)
(setq c-default-style "linux")
(setq-default c-basic-offset 4)

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

(defun makefile-tabs-are-less-evil ()
  (setq ethan-wspace-errors (remove 'tabs ethan-wspace-errors))
(add-hook 'makefile-mode-hook 'makefile-tabs-are-less-evil))

(add-hook 'sml-mode-hook 'no-final-newline t)
(add-hook 'fsharp-mode-hook 'no-final-newline t)

(defadvice ruby-mode-variables (after reset-final-newline)
  "Reset final-newline that ruby-mode enforces but conflicts with ethan-wspace."
  (setq require-final-newline nil)
  (setq mode-require-final-newline nil))
(ad-activate 'ruby-mode-variables)

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
(evil-define-key 'normal omnisharp-mode-map (kbd "\M-g") 'omnisharp-go-to-definition)

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
       (add-hook 'confluence-mode-hook '(lambda () (local-set-key "\C-j" 'confluence-newline-and-indent))))))

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

;;; mode hooks

(add-hook 'org-mode-hook 'my-org-mode-hook t)
(defun my-org-mode-hook ()
  (setq org-src-fontify-natively t))

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'global-company-mode-hook 'my-global-company-mode-hook t)
(defun my-global-company-mode-hook ()
  (define-key company-active-map (kbd "j") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "k") 'company-select-previous-or-abort))

(add-hook 'lisp-mode-hook 'my-lisp-mode-hook t)
(defun my-lisp-mode-hook ()
  (rainbow-delimiters-mode 1)
  )

(add-hook 'racket-mode-hook 'my-racket-mode-hook t)
(defun my-racket-mode-hook()
  (fci-mode))

(add-hook 'c-mode-common-hook 'my-c-mode-hook t)
(defun my-c-mode-hook ()
  (setq tab-width 4)
  (setq-default indent-tabs-mode nil)
  (c-set-offset 'substatement-open 0)
  (company-mode)
  (local-set-key (kbd "<tab>") 'company-complete-common)
  ;; (yas-minor-mode 1)
  (fci-mode)
  (rainbow-delimiters-mode 1)
  )

(add-hook 'csharp-mode-hook 'my-csharp-mode-hook t)
(defun my-csharp-mode-hook ()
  (electric-pair-mode 0)
  (add-to-list 'company-backends 'company-omnisharp)
  (c-set-style "c#")
  (omnisharp-mode)
  (flycheck-mode)
  (local-set-key "\M-g" 'omnisharp-go-to-definition)
  )

(add-hook 'fsharp-mode-hook 'my-fsharp-mode-hook t)
(defun my-fsharp-mode-hook ()
  (omnisharp-mode)
  (rainbow-delimiters-mode 1)
  )

;; (add-hook 'ruby-mode-hook 'my-ruby-mode-hook t)
;; (defun my-ruby-mode-hook ()
;;   )

(defun my-haskell-mode-hook ()
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indentation)
  ;; (turn-on-haskell-indent)
  ;; (turn-on-haskell-simple-indent)

  (interactive-haskell-mode)
  ;; (setq tab-stop-list '(1 3 5))
  (turn-on-hi2)
  (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
    (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
    (add-to-list 'exec-path my-cabal-path))
  (custom-set-variables '(haskell-tags-on-save t))

  (add-to-list 'company-backends 'company-ghc)
  (custom-set-variables '(company-ghc-show-info t))
  (rainbow-delimiters-mode 1)
)

(add-hook 'haskell-mode-hook 'my-haskell-mode-hook t)
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))
(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t))
(eval-after-load 'haskell-mode '(progn
                                  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
                                  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
                                  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
                                  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))
(eval-after-load 'haskell-cabal '(progn
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
                                   (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

(custom-set-variables '(haskell-process-type 'cabal-repl))

(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile))
(eval-after-load 'haskell-cabal
  '(define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile))
