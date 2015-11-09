(require 'cl)

(setq ad-redefinition-action 'accept)

(let ((theme 'molokai))
  (if (display-graphic-p)
      (progn (load-theme theme t)
             (when (eq theme 'molokai)
               (set-face-foreground 'font-lock-comment-face "azure4")))
    (load-theme 'molokai t)))

;;; airline is too slow on Mac OS X
(if (not (eq system-type 'darwin))
    (load-theme 'airline-powerlineish t))

;;; Set name and email
(require 's)
(let ((user-full-name "Jonas Collberg"))
  (setq user-mail-address
        (concat (s-replace " " "." (downcase user-full-name)) "@"
                (if (eq t (compare-strings (system-name) nil nil
                                           "orz-lap01" nil nil t))
                    "orzone.com"
                  "gmail.com"))))

(global-set-key (kbd "<f4>")
                (lambda ()
                  (interactive)
                  (start-process "gvim" nil
                                 "gvim"
                                 (format "+%d" (line-number-at-pos))
                                 (buffer-file-name))))

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

(tool-bar-mode -1)
(global-auto-revert-mode)
(global-font-lock-mode)
(show-paren-mode)
(electric-pair-mode)
(rainbow-mode)
(global-linum-mode)
(when (display-graphic-p) (global-hl-line-mode))

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

(load-library "iso-transl")
(setq system-time-locale "C")
(setq display-time-string-forms '(24-hours ":" minutes))
(display-time-mode)

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

(global-set-key (kbd "C-x a r") 'align-regexp)
(defadvice align-regexp (around align-regexp-with-spaces activate compile)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))

(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)
(global-set-key (kbd "C-c C-b") 'help-go-back)
(global-set-key (kbd "C-c C-f") 'help-go-forward)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(setq cider-show-error-buffer 'nil)
(setq ecb-tip-of-the-day nil)

(require 'recentf)
(recentf-mode)
(setq recentf-max-menu-items 25)

(require 'omnisharp)
(setq omnisharp-company-do-template-completion t)

(provide 'init-common)
