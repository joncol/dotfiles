(require 'cl)

;;; Avoid the empty (custom-set-faces) at end of init.el.
(setq custom-file (expand-file-name (concat user-emacs-directory "custom.el")))

(setq ad-redefinition-action 'accept)

;;; Set name and email
(require 's)
(let ((user-full-name "Jonas Collberg"))
  (setq user-mail-address
        (concat (s-replace " " "." (downcase user-full-name)) "@"
                (if (jco/at-office-p)
                    "orzone.com"
                  "mykolab.com"))))

(setq gnus-init-file (concat user-emacs-directory "lisp/init-gnus.el"))

(global-set-key (kbd "<f4>")
                (lambda ()
                  (interactive)
                  (start-process "gvim" nil
                                 "gvim"
                                 (format "+%d" (line-number-at-pos))
                                 (buffer-file-name))))

(global-set-key (kbd "S-<f6>")
                (lambda ()
                  (interactive)
                  (kill-new (buffer-file-name))))

(global-set-key (kbd "<f8>")
                (lambda ()
                  (interactive)
                  (insert user-full-name)))

(global-set-key (kbd "S-<f8>")
                (lambda ()
                  (interactive)
                  (insert user-mail-address)))

(lexical-let ((init-file (concat user-emacs-directory "init.el")))
  (global-set-key (kbd "<f9>")
                  (lambda ()
                    (interactive)
                    (find-file init-file)))

  (global-set-key (kbd "S-<f9>")
                  (lambda ()
                    (interactive)
                    (load-file init-file))))

(global-set-key (kbd "<f12>") (lambda ()
                                (interactive)
                                (message "Current major mode: %s" major-mode)))

(global-set-key (kbd "C-c t f") 'toggle-frame-fullscreen)

(global-set-key (kbd "M-w") 'ace-window)

(tool-bar-mode -1)
(global-auto-revert-mode)
(global-font-lock-mode)
(electric-pair-mode)
(rainbow-mode)
(global-linum-mode)
;; (global-subword-mode)
(setq helm-gtags-path-style 'absolute)
(setq sentence-end-double-space nil)

(when (display-graphic-p)
  (global-hl-line-mode)
  (show-paren-mode))

(modify-syntax-entry ?_ "w") ;; do not treat "_" as a word separator
(edit-server-start)
(setq ring-bell-function 'ignore)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(electric-indent-mode)
(global-set-key (kbd "RET")
                (lambda ()
                  (interactive)
                  (delete-trailing-whitespace (line-beginning-position)
                                              (line-end-position))
                  (newline-and-indent)))

;;; autocomplete
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)
(auto-complete-mode)
(setq ac-ignore-case 'smart)
(setq company-dabbrev-ignore-case 'keep-prefix)
(setq company-dabbrev-code-ignore-case nil)
(setq company-dabbrev-downcase nil)

(setq scroll-step           1
      scroll-conservatively 10000)
(setq inhibit-startup-message t)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq scroll-margin 4)

(load-library "iso-transl")
(setq system-time-locale "C")
(setq display-time-string-forms '(24-hours ":" minutes))
(display-time-mode)

(global-set-key (kbd "C-x a r") 'align-regexp)
(defadvice align-regexp (around align-regexp-with-spaces activate compile)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))

(jco/define-bindings Info-mode-map
                     '(("<tab>" . Info-next-reference)
                       ("<backtab>" . Info-prev-reference)))

(jco/define-bindings help-mode-map
                     '(("<tab>" . forward-button)
                       ("<backtab>" . backward-button)))

(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)
(global-set-key (kbd "C-c C-b") 'help-go-back)
(global-set-key (kbd "C-c C-f") 'help-go-forward)

(when (memq window-system '(mac ns))
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(let ((my-bin-path (expand-file-name "~/.local/bin")))
  (setenv "PATH" (concat (getenv "PATH") ":" my-bin-path))
  (add-to-list 'exec-path my-bin-path t))

(setq cider-show-error-buffer 'nil)
(setq ecb-tip-of-the-day nil)

(require 'recentf)
(recentf-mode)
(setq recentf-max-menu-items 25)

(setq fortune-dir "/usr/share/games/fortunes")
(setq fortune-file "/usr/share/games/fortunes")

(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c C-r"))
;; (setq guide-key/popup-window-position "right")
(guide-key-mode)
(diminish 'guide-key-mode)

(put 'erase-buffer 'disabled nil)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(setq compilation-scroll-output t)

(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

(global-anzu-mode)
(diminish 'anzu-mode)

(require 'string-inflection)
(global-unset-key (kbd "C-q"))
(global-set-key (kbd "C-q C-u") 'string-inflection-all-cycle)

(global-set-key (kbd "C-=") 'er/expand-region)

(fancy-narrow-mode)
(ace-link-setup-default (kbd "f"))

(require 'fuzzy)
(turn-on-fuzzy-isearch)

(require 'qmake-mode)
(require 'iedit)

(require 'diminish)
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "company" '(diminish 'company-mode))

(require 'golden-ratio)
(golden-ratio-mode)
(diminish 'golden-ratio-mode)

(provide 'init-common)
