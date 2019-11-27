;;; #init-evil.el --- Evil config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(defun jco/bind-exit-insert-mode (first-key second-key)
  "Add binding to exit insert mode using FIRST-KEY followed by SECOND-KEY."
  (define-key evil-insert-state-map (char-to-string first-key)
    #'jco/maybe-exit)
  (evil-define-command jco/maybe-exit ()
    :repeat change
    (interactive)
    (let ((modified (buffer-modified-p)))
      (insert (char-to-string first-key))
      (let ((evt (read-event (format "Insert %c to exit insert state" ?j)
                             nil 0.25)))
        (cond
         ((null evt) (message ""))
         ((and (integerp evt) (char-equal evt second-key))
          (delete-char -1)
          (set-buffer-modified-p modified)
          (push 'escape unread-command-events))
         (t (setq unread-command-events (append unread-command-events
                                                (list evt)))))))))

(defun bind-window-keys (keymap)
  "Apply windmove key bindings to KEYMAP."
  (bind-keys :map keymap
    ("C-w h"   . windmove-left)
    ("C-w C-h" . windmove-left)
    ("C-w j"   . windmove-down)
    ("C-w C-j" . windmove-down)
    ("C-w k"   . windmove-up)
    ("C-w C-k" . windmove-up)
    ("C-w l"   . windmove-right)
    ("C-w C-l" . windmove-right)
    ("C-w v"   . evil-window-vsplit)
    ("C-w C-v" . evil-window-vsplit)
    ("C-w s"   . evil-window-split)
    ("C-w C-s" . evil-window-split)
    ("C-w c"   . evil-window-delete)
    ("C-w C-c" . evil-window-delete)))

(use-package evil-leader
  :init
  ;; Enable global-evil-leader-mode before evil-mode, to make leader key work
  ;; in *Messages* and *scratch* buffers.
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (setq evil-leader/in-all-states t))

(use-package evil
  :init
  (evil-mode)

  :config
  (unbind-key "C-n" evil-insert-state-map) ;; Want company-select-next-or-abort.
  (unbind-key "C-p" evil-insert-state-map) ;; Want company-select-next-or-abort.
  (unbind-key "C-t" evil-normal-state-map) ;; Want transpose-chars instead.
  (setq evil-want-C-w-in-emacs-state t)

  ;; Set other modes than evil-mode for the following modes.
  (dolist (mode-map '((ag-mode                   . emacs)
                      (cider-browse-ns-mode      . emacs)
                      (cider-macroexpansion-mode . emacs)
                      (cider-repl-history-mode   . emacs)
                      (cider-stacktrace-mode     . emacs)
                      (cider-test-report-mode    . emacs)
                      (compilation-mode          . motion)
                      (dashboard-mode            . emacs)
                      (diff-mode                 . emacs)
                      (dired-mode                . emacs)
                      (doc-view-mode             . emacs)
                      (elfeed-search-mode        . emacs)
                      (elfeed-show-mode          . emacs)
                      (esup-mode                 . emacs)
                      (eww-mode                  . emacs)
                      (fireplace-mode            . emacs)
                      (flycheck-error-list-mode  . emacs)
                      (forge-pullreq-list-mode   . emacs)
                      (git-commit-mode           . insert)
                      (git-rebase-mode           . emacs)
                      (ivy-occur-mode            . emacs)
                      (profiler-report-mode      . emacs)
                      (rtags-mode                . emacs)
                      (sdcv-mode                 . emacs)
                      (sesman-browser-mode       . emacs)
                      (sx-question-list-mode     . emacs)
                      (sx-question-mode          . emacs)
                      (term-mode                 . emacs)
                      (xkcd-mode                 . emacs)
                      (xref--xref-buffer-mode    . emacs)
                      (yagist-list-mode          . emacs)))
    (evil-set-initial-state (car mode-map) (cdr mode-map)))

  (defadvice org-goto (around make-it-evil activate)
    "Disable evil-mode mappings for org-goto."
    (let ((evil-emacs-state-modes (cons 'org-mode evil-emacs-state-modes)))
      ad-do-it
      (evil-change-state evil-state)))

  (jco/move-key (kbd "RET") evil-motion-state-map evil-normal-state-map)
  (jco/move-key " " evil-motion-state-map evil-normal-state-map)

  (define-key evil-normal-state-map (kbd "+") 'rotate-word-at-point)
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (define-key evil-insert-state-map (kbd "C-k") nil) ;; Conflicts with Company.

  (setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))
  (when (display-graphic-p)
    (define-key evil-normal-state-map (kbd "C-i") 'evil-jump-forward))

  (jco/define-bindings 'evil-window-map
                       '(("C-h" . windmove-left)
                         ("C-j" . windmove-down)
                         ("C-k" . windmove-up)
                         ("C-l" . windmove-right)))

  (jco/bind-exit-insert-mode ?l ?h) ;; Colemak specific
  (setq evil-flash-delay 3600))

(use-package evil-exchange
  :init
  (evil-exchange-cx-install))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode))

(use-package evil-goggles
  :config
  (evil-goggles-mode))

(use-package evil-search-highlight-persist
  :init
  (global-evil-search-highlight-persist t))

(add-hook 'edebug-mode-hook 'evil-normalize-keymaps)

(use-package evil-surround
  :init
  (global-evil-surround-mode)
  :config
  (dolist (hook '(emacs-lisp-mode-hook erc-mode-hook org-mode-hook))
    (add-hook hook
              (lambda ()
                (push '(?` . ("`" . "'")) evil-surround-pairs-alist)))))

(evil-leader/set-key "v d" 'vc-diff)
(evil-leader/set-key "D" 'ediff-current-file)

(defun jco/remove-search-highlights ()
  "Remove any persisted highlighted search results."
  (interactive)
  (evil-search-highlight-persist-remove-all))

(jco/define-bindings evil-search-highlight-persist-map
                     '(("C-x SPC" . jco/remove-search-highlights)
                       ("C-x C-SPC" . jco/remove-search-highlights)))

(evil-leader/set-key "g g" 'ggtags-find-tag-dwim)

(evil-leader/set-key "SPC" 'cycle-spacing)
(evil-leader/set-key "RET" 'delete-blank-lines)

(use-package evil-nerd-commenter
  :demand t
  :config
  (global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
  (evil-leader/set-key
    "c i" 'evilnc-comment-or-uncomment-lines
    "c l" 'evilnc-quick-comment-or-uncomment-to-the-line
    "c c" 'evilnc-copy-and-comment-lines
    "c p" 'evilnc-comment-or-uncomment-paragraphs
    "c v" 'evilnc-toggle-invert-comment-line-by-line
    "c b" 'comment-box))

(cl-defun jco/move-window-to-bottom (&optional (height 20))
  "Move window to bottom and make it be HEIGHT lines high.
Useful for REPL windows."
  (interactive)
  (evil-window-move-very-bottom)
  (evil-window-set-height height))

(evil-leader/set-key "V" 'jco/vcs-status)

(evil-leader/set-key "n n" 'narrow-to-defun)
(evil-leader/set-key "n r" 'narrow-to-region)
(evil-leader/set-key "n s" 'org-narrow-to-subtree)
(evil-leader/set-key "n w" 'widen)

(evil-leader/set-key "i n" '(lambda ()
                              (interactive)
                              (when (eq evil-state 'normal)
                                (forward-char))
                              (insert user-full-name)))

(evil-leader/set-key "i m" '(lambda ()
                              (interactive)
                              (when (eq evil-state 'normal)
                                (forward-char))
                              (insert user-mail-address)))

(evil-leader/set-key "e f" 'jco/what-face)
(evil-leader/set-key "e w" 'ace-window)

(provide 'init-evil)

;;; init-evil.el ends here
