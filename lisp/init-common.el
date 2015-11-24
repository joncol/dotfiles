(require 'cl)

(setq ad-redefinition-action 'accept)

(let ((theme 'molokai))
  (if (display-graphic-p)
      (progn (load-theme theme t)
             (when (eq theme 'molokai)
               (set-face-foreground 'font-lock-comment-face "azure4")))
    (load-theme 'cyberpunk t)))

(if (eq system-type 'darwin)
    (nyan-mode)
  (load-theme 'airline-powerlineish t)) ;;; airline is too slow on Mac OS X

;;; Set name and email
(require 's)
(let ((user-full-name "Jonas Collberg"))
  (setq user-mail-address
        (concat (s-replace " " "." (downcase user-full-name)) "@"
                (if (jco/at-office-p)
                    "orzone.com"
                  "gmail.com"))))

(setq gnus-init-file (concat user-emacs-directory "lisp/init-gnus.el"))

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

(global-set-key (kbd "C-x a r") 'align-regexp)
(defadvice align-regexp (around align-regexp-with-spaces activate compile)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))

(jco/define-bindings Info-mode-map
                     '(("<tab>" . Info-next-reference)
                       ("<backtab>" . Info-prev-reference)))

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

(setq fortune-dir "/usr/share/games/fortunes")
(setq fortune-file "/usr/share/games/fortunes")

(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
;; (setq guide-key/popup-window-position "right")
(guide-key-mode)

(defun jco/json-lint ()
  "Pretty format JSON."
  (interactive)
  (save-restriction
    (widen)
    (shell-command-on-region (point-min) (point-max) "python -m json.tool"
                             t t)))

(defun jco/underline-line (&optional char)
  "Underline the current line with a character (\"-\" is the default)."
  (interactive)
  (let ((line-length (jco/get-line-length)))
    (end-of-line)
    (insert (concat "\n" (make-string line-length (or char ?-))))
    (beginning-of-line)))

(global-set-key (kbd "<f7>") 'jco/underline-line)
(global-set-key (kbd "<S-f7>") (lambda () (interactive) (jco/underline-line ?=)))

(defun jco/get-line-length (&optional print-message)
  "Get the length of the current line."
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (let ((line-start-pos (point)))
      (end-of-line)
      (let ((line-length (- (point) line-start-pos)))
        (when print-message (message (format "Current line length: %d"
                                             line-length)))
        line-length))))

(defun jco/read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(provide 'init-common)
