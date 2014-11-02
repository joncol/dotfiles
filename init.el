(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(add-to-list 'load-path "~/elisp")

(setq ring-bell-function 'ignore)
(global-font-lock-mode 1)
(show-paren-mode 1)

;;; color theme
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
(color-theme-solarized 'light)
;;(color-theme-molokai)

(when window-system (set-frame-size (selected-frame) 160 80))

(require 'fill-column-indicator)
(setq-default fill-column 80)
(setq fci-rule-width 1)
;;(setq fci-rule-color "#ff0000")
(add-hook 'after-change-major-mode-hook 'fci-mode)

(evil-mode 1)

;;; autocomplete
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

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

;;; ethan-wspace
(setq require-final-newline nil)
(setq mode-require-final-newline nil)
(require 'ethan-wspace)

(global-ethan-wspace-mode 1)

(add-hook 'c-mode-common-hook
          (lambda () (setq indent-tabs-mode t)))

(defun makefile-tabs-are-less-evil ()
  (setq ethan-wspace-errors (remove 'tabs ethan-wspace-errors))
(add-hook 'makefile-mode-hook 'makefile-tabs-are-less-evil))

(add-hook 'sml-mode-hook
          (lambda ()
            (setq require-final-newline nil)
            (setq mode-require-final-newline nil)))


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

(yas-global-mode 1)
