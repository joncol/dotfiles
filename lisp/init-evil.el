(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-mode)
(global-evil-matchit-mode)
(global-evil-surround-mode)
(global-evil-search-highlight-persist t)

(eval-after-load "evil"
  ;; set other modes than evil-mode for the following modes
  (dolist (mode-map '((ag-mode               . emacs)
                      (comint-mode           . emacs)
                      (diff-mode             . emacs)
                      (dired-mode            . emacs)
                      (eshell-mode           . emacs)
                      (eww-mode              . emacs)
                      (git-commit-mode       . insert)
                      (git-rebase-mode       . emacs)
                      (paradox-menu-mode     . emacs)
                      (sx-question-list-mode . emacs)
                      (sx-question-mode      . emacs)
                      (term-mode             . emacs)
                      (xkcd-mode             . emacs)))
    (evil-set-initial-state (car mode-map) (cdr mode-map))))

(defadvice org-goto (around make-it-evil activate)
  (let ((orig-state evil-state)
        (evil-emacs-state-modes (cons 'org-mode evil-emacs-state-modes)))
    ad-do-it
    (evil-change-state orig-state)))

(jco/move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(jco/move-key evil-motion-state-map evil-normal-state-map " ")

(jco/define-bindings evil-normal-state-map '(("+" . rotate-word-at-point)))

(require 'apropos)
(require 'monky)

(custom-set-variables
 '(evil-want-C-w-in-emacs-state t))

(jco/define-bindings 'evil-window-map
                     '(("C-h" . windmove-left)
                       ("C-j" . windmove-down)
                       ("C-k" . windmove-up)
                       ("C-l" . windmove-right)))

;; Stop SLIME's REPL from grabbing DEL, which is annoying when backspacing over
;; a '('
(add-hook 'slime-repl-mode-hook
          (lambda ()
            (define-key slime-repl-mode-map
              (read-kbd-macro paredit-backward-delete-key) nil)))

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

(setq evil-flash-delay 3600)

(defun run-process (program &rest args)
  (apply 'start-process program nil program args))

(defun run-on-current-buffer (program &rest args)
  (apply 'start-process program nil program
         (append args (list (buffer-file-name)))))

(evil-leader/set-key "t a"
  (lambda ()
    (interactive)
    (run-on-current-buffer "thg" "annotate")))

(evil-leader/set-key "t c"
  (lambda ()
    (interactive)
    (run-process "thg" "ci")))

(evil-leader/set-key "t C"
  (lambda ()
    (interactive)
    (run-on-current-buffer "thg" "ci")))

(evil-leader/set-key "t l"
  (lambda ()
    (interactive)
    (run-on-current-buffer "thg" "log")))

(evil-leader/set-key "t d"
  (lambda ()
    (interactive)
    (if (eq system-type 'darwin)
        (run-on-current-buffer "hg" "opendiff")
      (run-on-current-buffer "thg" "vdiff"))))

(evil-leader/set-key "t s"
  (lambda ()
    (interactive)
    (run-process "thg" "st")))

(evil-leader/set-key "t S"
  (lambda ()
    (interactive)
    (run-on-current-buffer "thg" "shelve")))

(evil-leader/set-key "d" 'vc-diff)

(global-set-key (kbd "C-x C-SPC") 'evil-search-highlight-persist-remove-all)

(evil-leader/set-key "g g" 'ggtags-find-tag-dwim)

(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

(evil-leader/set-key "SPC" 'cycle-spacing)
(evil-leader/set-key "RET" 'delete-blank-lines)

;;; evil-nerd-commenter

(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)

(evil-leader/set-key
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cv" 'evilnc-toggle-invert-comment-line-by-line)

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
;; (evil-leader/set-key "m s" 'helm-multi-swoop)
(global-set-key (kbd "C-M-S-i") 'helm-multi-swoop-all)
;; (evil-leader/set-key "m S" 'helm-multi-swoop-all)

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

(define-key projectile-command-map (kbd "s a") #'helm-ag-project-root)

;;; avy
(evil-leader/set-key "f" 'evil-avy-goto-char)
(evil-leader/set-key "#" 'evil-avy-goto-line)
(evil-leader/set-key "F" 'evil-avy-goto-word-or-subword-1)
(setq avy-case-fold-search nil)

(evil-leader/set-key "x f" 'jco/xmllint-format-buffer)

(require 'fancy-narrow)
(fancy-narrow-mode)
(diminish 'fancy-narrow-mode)
(evil-leader/set-key "n" 'fancy-narrow-to-defun)
(evil-leader/set-key "w" 'fancy-widen)

(evil-leader/set-key "i n" '(lambda ()
                              (interactive)
                              (insert user-full-name)))

(evil-leader/set-key "i m" '(lambda ()
                              (interactive)
                              (insert user-mail-address)))

;; (evil-leader/set-key "m m" 'mu4e)

;;; yasnippet
(evil-leader/set-key "TAB" 'yas-insert-snippet)

(provide 'init-evil)
