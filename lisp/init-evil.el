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
                               nil 0.5)))
          (cond
           ((null evt) (message ""))
           ((and (integerp evt) (char-equal evt second-key))
            (delete-char -1)
            (set-buffer-modified-p modified)
            (push 'escape unread-command-events))
           (t (setq unread-command-events (append unread-command-events
                                                  (list evt)))))))))

(use-package evil-leader
  :init
  ;; Enable global-evil-leader-mode before evil-mode, to make leader key work
  ;; in *Messages* and *scratch* buffers.
  (global-evil-leader-mode)
  (evil-leader/set-leader ","))

(use-package evil
  :init
  (evil-mode)

  :config
  ;; Want transpose-chars instead.
  (unbind-key "C-t" evil-normal-state-map)
  (setq evil-want-C-w-in-emacs-state t)

  ;; Set other modes than evil-mode for the following modes.
  (dolist (mode-map '((ag-mode                  . emacs)
                      (comint-mode              . emacs)
                      (cider-browse-ns-mode     . emacs)
                      (cider-test-report-mode   . emacs)
                      (dashboard-mode           . emacs)
                      (diff-mode                . emacs)
                      (dired-mode               . emacs)
                      (eshell-mode              . emacs)
                      (eww-mode                 . emacs)
                      (fireplace-mode           . emacs)
                      (flycheck-error-list-mode . emacs)
                      (git-commit-mode          . insert)
                      (git-rebase-mode          . emacs)
                      (profiler-report-mode     . emacs)
                      (rtags-mode               . emacs)
                      (sdcv-mode                . emacs)
                      (sx-question-list-mode    . emacs)
                      (sx-question-mode         . emacs)
                      (term-mode                . emacs)
                      (xkcd-mode                . emacs)))
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

  (setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))
  (define-key evil-normal-state-map (kbd "C-i") 'evil-jump-forward)

  ;; (require 'apropos)

  (jco/define-bindings 'evil-window-map
                       '(("C-h" . windmove-left)
                         ("C-j" . windmove-down)
                         ("C-k" . windmove-up)
                         ("C-l" . windmove-right)))

  (if jco/use-colemak
      (jco/bind-exit-insert-mode ?l ?h)
    (jco/bind-exit-insert-mode ?k ?j))

  (setq evil-flash-delay 3600))

(use-package evil-exchange
  :init
  (evil-exchange-cx-install))

(use-package evil-matchit
  :init
  (global-evil-matchit-mode))

(use-package evil-goggles
  :diminish evil-goggles-mode
  :config
  (evil-goggles-mode)
  (setq evil-goggles-pulse t))

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
(evil-leader/set-key "g o" 'google-this)

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

(evil-leader/set-key "V" 'jco/vcs-status)

(evil-leader/set-key "q" 'kill-buffer)
(evil-leader/set-key "n n" 'narrow-to-defun)
(evil-leader/set-key "n r" 'narrow-to-region)
(evil-leader/set-key "n s" 'org-narrow-to-subtree)
(evil-leader/set-key "n w" 'widen)

(evil-leader/set-key "i n" '(lambda ()
                              (interactive)
                              (insert user-full-name)))

(evil-leader/set-key "i m" '(lambda ()
                              (interactive)
                              (insert user-mail-address)))

;; (evil-leader/set-key "m m" 'mu4e)

(evil-leader/set-key "e f" 'jco/what-face)
(evil-leader/set-key "e w" 'ace-window)

(provide 'init-evil)

;;; init-evil.el ends here
