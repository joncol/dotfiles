(let ((my-theme '"doom-1337"))
(defvar jco/theme)
(setq jco/theme (intern my-theme))
)

(defvar bootstrap-version)
(let ((bootstrap-file
               (expand-file-name "straight/repos/straight.el/bootstrap.el"
                                 user-emacs-directory))
            (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
          (with-current-buffer
                    (url-retrieve-synchronously
                               "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                                        'silent 'inhibit-cookies)
                          (goto-char (point-max))
                                (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq-default flycheck-emacs-lisp-load-path load-path)

(defvar evil-want-C-i-jump nil)

(defun apply-ansi-colors ()
  "Apply ANSI colors on region."
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun get-ip-address (&optional dev)
  "Get the IP-address for device DEV (default: eth0)."
  (let ((dev (if dev dev "eth0")))
    (format-network-address (car (network-interface-info dev)) t)))

(defun jco/update-dotfiles ()
  "Get the latest dotfiles from source control."
  (shell-process-pushd "~/dotfiles")
  (magit-pull-from-pushremote nil)
  (shell-process-popd "1"))

(defun jco/at-office-p (&optional print-message)
  "Check whether at the office.
If PRINT-MESSAGE is true, a message will be printed indicating the result."
  (interactive "P")
  (let ((result (member system-name '("jco-thinkpad"))))
    (if print-message
        (message (if result
                     "You're at the office"
                   "You're not at the office"))
      result)))

(defun jco/at-digitalocean-p (&optional print-message)
  "Check whether on the DigitalOcean server.
If PRINT-MESSAGE is true, a message will be printed indicating the result."
  (interactive "P")
  (let ((result (string-equal "162.243.220.203" (get-ip-address))))
    (if print-message
        (message (if result
                     "You're on DigitalOcean"
                   "You're not on DigitalOcean"))
      result)))

(defun jco/read-lines (file-path)
  "Return a list of lines of a file at FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defun jco/define-bindings (keymap binding-alist)
  "Define keys for KEYMAP given a BINDING-ALIST."
  (dolist (p binding-alist)
    (let ((key (car p))
          (command (cdr p)))
      (define-key keymap (kbd key) command))))

(defun jco/move-key (key keymap-from keymap-to)
  "Move KEY binding from KEYMAP-FROM to KEYMAP-TO."
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))

(defun jco/common-prog ()
  "Common setup for programming modes."
  (when (display-graphic-p)
    ;; (hl-line-mode)
    (rainbow-delimiters-mode)
    (rainbow-mode t))
  (setq require-final-newline nil)
  (ethan-wspace-mode)
  (modify-syntax-entry ?_ "w") ; do not treat "_" as a word separator
  (hs-minor-mode))

(defun jco/underline-line (&optional char)
  "Underline the current line with a character CHAR (\"-\" is the default)."
  (interactive)
  (let ((line-length (jco/get-line-length)))
    (end-of-line)
    (insert (concat "\n" (make-string line-length (or char ?-))))
    (beginning-of-line)))

(global-set-key (kbd "<f7>") 'jco/underline-line)
(global-set-key (kbd "<S-f7>") (lambda () (interactive) (jco/underline-line ?=)))

(defun jco/get-line-length (&optional print-message)
  "Get the length of the current line.
If PRINT-MESSAGE is non-nil, print a message"
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (let ((line-start-pos (point)))
      (end-of-line)
      (let ((line-length (- (point) line-start-pos)))
        (when print-message (message (format "Current line length: %d"
                                             line-length)))
        line-length))))

(defun jco/capitalize-first-char (&optional string)
  "Capitalize the first characer of STRING."
  (when (and string (> (length string) 0))
   (let ((first-char (substring string 0 1))
         (rest-str (substring string 1)))
     (concat (capitalize first-char) rest-str))))

(defun jco/downcase-first-char (string)
  "Downcase the first character of STRING."
  (concat (downcase (substring string 0 1)) (substring string 1)))

(defun jco/cpp-insert-class-name ()
  "Insert the class name corresponding to the name of the current buffer."
  (interactive)
  (insert (jco/cpp-class-name)))

(defun jco/irc-account ()
  "Return a cons cell of username and password."
  (if (file-exists-p "~/.irc")
    (let ((lines (jco/read-lines "~/.irc")))
      (cons (car lines) (cadr lines)))
    '("" . "")))

(defun jco/cmake-target-string ()
  "Get target string for CMake."
  (let ((proj (jco/cmake-project-name)))
    (if (string= proj "src")
        ""
      (concat "--target " proj))))

(defun jco/cmake-project-name ()
  "Get name of CMake project.
Traverses the directory hierarchy upwards and looks for the first
CMakeLists.txt file."
  (let ((dir (locate-dominating-file (buffer-file-name) "CMakeLists.txt")))
    (car (last (f-split dir)))))

(defun jco/vcs-status ()
  "Run either monky-status or magit-status, depending on the kind of repo."
  (interactive)
  (cl-case (projectile-project-vcs)
    (git (magit-status))
    (hg (monky-status))))

(defun jco/vcs-update ()
  "Run either `hg pull -u' or `git pull', depending on the kind of repo."
  (interactive)
  (cl-case (projectile-project-vcs)
    (git (magit-pull "origin/master" nil))
    (hg (monky-hg-command "pull -u"))))

(defun jco/what-face (pos)
  "Determine the face at the point POS."
  (interactive "d")
  (let ((g global-hl-line-mode))
    (global-hl-line-mode -1)
    (let ((face (or (get-char-property (point) 'read-face-name)
                    (get-char-property (point) 'face))))
      (if face
          (message "Face: %s" face)
        (message "No face at %d" pos)))
    (when g
      (global-hl-line-mode 1))))

(defun jco/re-seq (regexp string)
  "Get a list of all regex-matches of REGEXP in STRING."
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      (nreverse matches))))

(defun jco/cmake-compile-command ()
  "Return compile command for building a project using CMake."
  (when (and (stringp (buffer-file-name))
             (projectile-project-p))
    (let ((sh (getenv "SHELL"))
          (target (jco/cmake-target-string)))
      (concat
       "cd " (projectile-project-root)
       (cond
        ((s-contains-p "fish" sh)
         (format "_build ;and cmake --build . %s -- -j4" target))
        ((eq system-type 'windows-nt)
         (format "_build && cmake --build . %s" target))
        (t (format
            "_build && cmake --build . %s -- -j4" target)))))))

(defun jco/display-ansi-colors ()
  "Display ansi color codes in buffer."
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun jco/tighten-braces ()
  "Fix formatting of braces.
Remove empty lines after opening brace and before closing brace."
  (interactive)
  (save-excursion
    ;; Remove empty line(s) after opening brace.
    (goto-char (point-min))
    (while (re-search-forward "{\n\n+" nil t)
      (replace-match "{\n"))

    ;; Remove empty line(s) before closing brace.
    (goto-char (point-min))
    (while (re-search-forward "\n\n+\\(\\s-*\\)}" nil t)
      (replace-match "\n\\1}"))))

(defun jco/run-process (program &rest args)
  "Start process PROGRAM with arguments ARGS."
  (apply 'start-process program nil program args))

(defun jco/run-on-current-buffer (program &rest args)
  "Start process PROGRAM with arguments ARGS on current buffer.
The filename of the current buffer is passed as the last argument to the process
invokation."
  (apply 'start-process program nil program
         (append args (list (buffer-file-name)))))

(defun jco/vim ()
  "Open current buffer in Vim."
  (interactive)
  (when (display-graphic-p)
    (start-process "nvim" nil
                   "alacritty"
                   "-e"
                   "tmux"
                   "new"
                   "nvim"
                   (format "+%d" (line-number-at-pos))
                   (buffer-file-name))))

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defun jco/find-buffers-by-regex (re)
  "Find the first buffer with a name matching RE."
  (seq-filter (lambda(b) (string-match re (buffer-name b))) (buffer-list)))

(defun jco/select-bottom-window ()
  "Select the bottommost window."
  (let ((bottom-window (selected-window))
        window-below)
    (while (setq window-below (window-in-direction 'below bottom-window))
      (setq bottom-window window-below))
    (select-window bottom-window)))

(defmacro jco/toggle-mode (mode)
  "Toggle between `MODE' and `normal-mode'."
  `(if (bound-and-true-p ,mode)
       (normal-mode)
     (funcall (quote ,mode))))

(defun jco/show-elec-pairs ()
  "Show the `electric-pair-pairs' and `electric-pair-text-pairs' lists."
  (interactive)
  (let ((s1 (mapcar (lambda (cs)
                      (cons (format "%c" (car cs)) (format "%c" (cdr cs))))
                    electric-pair-pairs))
        (s2 (mapcar (lambda (cs)
                      (cons (format "%c" (car cs)) (format "%c" (cdr cs))))
                    electric-pair-text-pairs)))
    (message "electric-pair-pairs: %s" (prin1-to-string s1))
    (message "electric-pair-text-pairs: %s" (prin1-to-string s2))))

(defun jco/bind-exit-insert-mode (first-key second-key)
  "Add binding to exit insert mode using FIRST-KEY followed by SECOND-KEY."
  (define-key evil-insert-state-map (char-to-string ?l)
    #'jco/maybe-exit)
  (evil-define-command jco/maybe-exit ()
    :repeat change
    (interactive)
    (let ((modified (buffer-modified-p)))
      (insert (char-to-string ?l))
      (let ((evt (read-event (format "Insert %c to exit insert state" ?j)
                             nil 0.25)))
        (cond
         ((null evt) (message ""))
         ((and (integerp evt) (char-equal evt ?h))
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
  :after evil
  :init
  ;; Enable global-evil-leader-mode before evil-mode, to make leader key work
  ;; in *Messages* and *scratch* buffers.
  (global-evil-leader-mode)
  :config
  (evil-leader/set-leader ",")
  (evil-leader/set-key "," 'evil-repeat-find-char-reverse)
  (setq evil-leader/in-all-states t)
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

  (evil-leader/set-key "x b" 'browse-url)
  (evil-leader/set-key "x n" #'normal-mode)
  (evil-leader/set-key "x w" 'woman)

  (evil-leader/set-key "g n"
    (lambda ()
      (interactive)
      (browse-url "https://github.com/notifications"))))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)

  :config
  (evil-mode)
  (evil-set-undo-system
   (if (version<= "28" emacs-version)
       'undo-redo
     'undo-tree))

  ;; Disable certain evil keys to make useful company-mode/embark bindings work.
  (unbind-key "C-n" evil-insert-state-map)
  (unbind-key "C-p" evil-insert-state-map)
  ;; (unbind-key "C-r" evil-insert-state-map)
  (unbind-key "C-s" evil-insert-state-map)
  (unbind-key "C-t" evil-normal-state-map)
  (unbind-key "C-." evil-normal-state-map) ; Using this for `embark-act'.
  (setq evil-want-C-w-in-emacs-state t)

  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "RET") nil)
    (define-key evil-motion-state-map (kbd "C-]") nil))

  ;; Set other modes than evil-mode for the following modes.
  (dolist (mode-map '((ag-mode                   . emacs)
                      (cider-browse-ns-mode      . emacs)
                      (compilation-mode          . motion)
                      (dashboard-mode            . emacs)
                      (deft-mode                 . emacs)
                      (doc-view-mode             . emacs)
                      (elfeed-search-mode        . emacs)
                      (elfeed-show-mode          . emacs)
                      (esup-mode                 . emacs)
                      (eww-mode                  . emacs)
                      (eww-history-mode          . emacs)
                      (fireplace-mode            . emacs)
                      (flycheck-error-list-mode  . motion)
                      (forge-pullreq-list-mode   . emacs)
                      (forge-topic-list-mode     . emacs)
                      (git-commit-mode           . insert)
                      (git-rebase-mode           . emacs)
                      (godoc-mode                . motion)
                      (profiler-report-mode      . emacs)
                      (sdcv-mode                 . emacs)
                      (sesman-browser-mode       . emacs)
                      (sx-question-list-mode     . emacs)
                      (sx-question-mode          . emacs)
                      (term-mode                 . emacs)
                      (xkcd-mode                 . emacs)
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

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-ediff
  :after evil)

(use-package evil-exchange
  :after evil
  :init
  (evil-exchange-cx-install))

(use-package evil-god-state
  :after evil
  :config
  (evil-define-key 'normal jco/my-keys-mode-map (kbd "SPC")
    'evil-execute-in-god-state)
  (evil-define-key 'god jco/my-keys-mode-map [escape] 'evil-god-state-bail))

(use-package evil-ledger
  :after (evil ledger-mode)
  :config
  (setq evil-ledger-sort-key "S")
  (add-hook 'ledger-mode-hook #'evil-ledger-mode))

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode))

(use-package evil-numbers
  :after evil
  :bind (("C-c +" . evil-numbers/inc-at-pt)
         ("C-c -" . evil-numbers/dec-at-pt)))

(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode))

(use-package evil-search-highlight-persist
  :after (evil facemenu)
  :bind (:map evil-search-highlight-persist-map
         ("C-x SPC" . evil-search-highlight-persist-remove-all)
         ("C-x C-SPC" . evil-search-highlight-persist-remove-all))
  :init
  (global-evil-search-highlight-persist t))

(add-hook 'edebug-mode-hook 'evil-normalize-keymaps)

(use-package evil-surround
  :after evil
  :init
  (global-evil-surround-mode)
  :config
  (dolist (hook '(emacs-lisp-mode-hook erc-mode-hook org-mode-hook))
    (add-hook hook
              (lambda ()
                (push '(?` . ("`" . "'")) evil-surround-pairs-alist)))))

(evil-leader/set-key "v d" 'vc-diff)
(evil-leader/set-key "D" 'ediff-current-file)

(evil-leader/set-key "g g" 'ggtags-find-tag-dwim)

(evil-leader/set-key "SPC" 'cycle-spacing)
(evil-leader/set-key "RET" 'delete-blank-lines)

(use-package evil-nerd-commenter
  :after evil
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

(use-package projectile
  :defer
  :init
  (projectile-mode)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (when (not (eq system-type 'windows-nt))
    (setq projectile-indexing-method 'native))
  (setq projectile-enable-caching t)
  (evil-leader/set-key ". c" #'projectile-commander)
  (def-projectile-commander-method ?a
    "Ag."
    (counsel-projectile-ag))
  (def-projectile-commander-method ?F
    "Git fetch."
    (magit-status)
    (if (fboundp 'magit-fetch-from-upstream)
        (call-interactively #'magit-fetch-from-upstream)
      (call-interactively #'magit-fetch-current)))
  (def-projectile-commander-method ?j
    "Jack-in."
    (let* ((opts (projectile-current-project-files))
           (file (ido-completing-read
                  "Find file: "
                  opts
                  nil nil nil nil
                  (car (cl-member-if
                        (lambda (f)
                          (string-match "core\\.clj\\'" f))
                        opts)))))
      (find-file (expand-file-name
                  file (projectile-project-root)))
      (run-hooks 'projectile-find-file-hook)
      (cider-jack-in))))

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)

  :bind (([C-iso-lefttab] . company-ispell)
         :map company-active-map
         ("C-j" . company-select-next-or-abort)
         ("C-k" . company-select-previous-or-abort)
         ("C-n" . company-select-next-or-abort)
         ("C-p" . company-select-previous-or-abort)
         ("<backtab>" . company-select-previous-or-abort)
         ("C-d" . company-show-doc-buffer)
         ("M-." . company-show-location)
         ("RET" . company-complete-selection))

  :config
  (add-to-list 'completion-styles 'initials t)
  (setq company-tooltip-align-annotations t)
  (setq company-dabbrev-ignore-case 'keep-prefix)
  (setq company-dabbrev-code-ignore-case nil)
  (setq company-dabbrev-downcase nil)

  (setq company-tooltip-limit 20)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0)
  (setq company-echo-delay 0)
  ;; (setq company-begin-commands '(self-insert-command))
  (setq company-transformers '(company-sort-by-occurrence)))

(use-package company-box
  :if (display-graphic-p)
  :hook (company-mode . company-box-mode))

(setq sentence-end-double-space nil)
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

(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq scroll-margin 4)

(load-library "iso-transl")
(setq system-time-locale "C")

(require 'time)
(setq display-time-string-forms '(24-hours ":" minutes))

(display-time-mode)

(defadvice align-regexp (around align-regexp-with-spaces activate compile)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))

(put 'narrow-to-region 'disabled nil)

(setq help-window-select t)

(dolist (hook '(help-mode-hook
                makefile-gmake-mode-hook
                scss-mode-hook
                sql-mode-hook))
  (add-hook hook
            (lambda ()
              ;; do not treat "-" as a word separator
              (modify-syntax-entry ?- "w"))))

(jco/define-bindings Info-mode-map
                     '(("<tab>"     . Info-next-reference)
                       ("<backtab>" . Info-prev-reference)))

(require 'help-mode)
(bind-keys :map help-mode-map
  ("<tab>"     . forward-button)
  ("<backtab>" . backward-button))

(jco/define-bindings help-mode-map '(("C-c C-b" . help-go-back)
                                     ("C-c C-f" . help-go-forward)))

(when (display-graphic-p)
  (global-unset-key (kbd "C-x C-z")))

(setq vc-follow-symlinks nil)

;;; Avoid the empty (custom-set-faces) at end of init.el.
(setq custom-file (expand-file-name (concat user-emacs-directory "custom.el")))
(load custom-file)

(setq native-comp-async-report-warnings-errors nil)
(setq ad-redefinition-action 'accept)

(setq-default explicit-shell-file-name "/bin/bash")

(define-minor-mode jco/my-keys-mode
  "Minor mode for my personal keybindings."
  :global t
  :keymap (make-sparse-keymap))

(define-key jco/my-keys-mode-map (kbd "C-x a r") 'align-regexp)

(add-hook 'jco/my-keys-mode-hook
          (lambda ()
            (evil-normal-state)))

(add-hook 'prog-mode-hook #'jco/common-prog)

(add-hook 'picture-mode
          (lambda ()
            (setq evil-undo-system 'undo-redo)))

(jco/my-keys-mode)

(setq save-interprogram-paste-before-kill t)
(setq focus-follows-mouse t)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(column-number-mode)
(menu-bar-mode -1)
(when (display-graphic-p)
  (scroll-bar-mode -1))
(tool-bar-mode -1)
(global-auto-revert-mode)
(add-to-list 'revert-without-query ".*\\.pdf\\'")
(global-font-lock-mode)
(setq select-enable-primary t)
(global-whitespace-mode)
(setq calendar-week-start-day 1)

(setq-default fill-column 80)
(global-display-fill-column-indicator-mode)

(global-hl-line-mode -1)

(setq epg-pinentry-mode 'ask)

(setq-default sh-indent-after-continuation 'always)

(defun prevent-whitespace-mode-for-magit ()
  (not (derived-mode-p 'magit-mode)))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-diff-options "-w")

(winner-mode)
(define-key jco/my-keys-mode-map (kbd "C-x C-j") (lambda ()
                                                   (interactive)
                                                   (dired ".")))

(define-key jco/my-keys-mode-map (kbd "C-c j")
  (lambda ()
    (interactive)
    (require 'calendar)
    (let* ((year (caddr (calendar-current-date)))
           (file-name (format "~/ledgers/%s.journal" year)))
      (find-file (expand-file-name file-name)))))

(when (eq system-type 'gnu/linux)
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "google-chrome-stable"))

(modify-syntax-entry ?_ "w") ;; do not treat "_" as a word separator

(defalias 'yes-or-no-p 'y-or-n-p)
(setq auto-save-default nil)
(setq make-backup-files nil)

(setq evil-motion-state-modes
      (append '(debugger-mode) evil-motion-state-modes))

(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)

(setq-default c-basic-offset 4)
(evil-leader/set-key-for-mode 'c-mode "a" #'ff-find-other-file)
(evil-leader/set-key-for-mode 'c++-mode "a" #'ff-find-other-file)

;; This is to get correct results for some UTF-8 characters in `psql' results.
(setenv "LC_ALL" "en_US.UTF-8")

(add-hook 'doc-view-mode-hook
          (lambda ()
            (when (fboundp 'nlinum-mode)
              (nlinum-mode -1))
            (define-key doc-view-mode-map "\C-w" 'evil-window-map)))

(add-hook 'eww-mode-hook
          (lambda ()
            (define-key eww-mode-map "\C-w" 'evil-window-map)))

(add-hook 'messages-buffer-mode-hook 'ansi-color-for-comint-mode-on)

(add-hook 'conf-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")))

(setq prolog-system 'gnu)

(add-hook 'prolog-mode-hook
          (lambda ()
            (setq evil-shift-width 4)))

(add-hook 'sql-mode-hook
          (lambda ()
            (setq evil-shift-width 4)
            (modify-syntax-entry ?- "w" sql-mode-syntax-table)))

(add-hook 'TeX-mode-hook
          (lambda ()
            (setq evil-shift-width 2)))

(add-hook 'octave-mode-hook
          (lambda ()
            (setq evil-shift-width 2)))

(add-hook 'markdown-mode-hook
          (lambda ()
            (auto-fill-mode)
            (setq evil-shift-width 4)
            (modify-syntax-entry ?- "w") ;; do not treat "_" as a word separator
            (footnote-mode)
            (turn-on-orgtbl)
            (setq markdown-gfm-use-electric-backquote nil)
            (setq markdown-spaces-after-code-fence 0)
            (setq markdown-gfm-additional-languages '("bash"))))

(defadvice view-emacs-news (after evil-motion-state-in-news-view
                                  activate compile)
  "Enable evil motion state."
  (evil-motion-state))

(defadvice view-emacs-problems (after evil-motion-state-in-problems-view
                                      activate compile)
  "Enable evil motion state."
  (evil-motion-state))

(jco/define-bindings minibuffer-inactive-mode-map
                     '(("C-n" . ido-next-match)
                       ("C-p" . ido-prev-match)))

(setq compilation-scroll-output t)

(add-to-list 'auto-mode-alist
             '("/\\(rfc\\|std\\)[0-9]+\\.txt\\'" . rfcview-mode))

(autoload 'rfcview-mode "rfcview" nil t)

(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(use-package pkgbuild-mode
  :defer)

(use-package pretty-hydra
  :defer)

(use-package project
  :after magit)

(use-package ace-isearch
  :disabled
  :config
  (global-ace-isearch-mode))

(use-package ace-link
  :defer
  :init
  (ace-link-setup-default "f"))

(use-package ace-window
  :defer
  :custom
  (aw-keys '(?a ?r ?s ?t ?n ?e ?i ?o))
  :init
  (evil-leader/set-key "w o" 'ace-window)
  (global-set-key [remap other-window] 'ace-window))

(use-package aggressive-indent
  :defer
  :config
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c-mode 'c++-mode 'java-mode)
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line))))))

(use-package alert
  :defer
  :custom
  (alert-default-style 'libnotify))

(use-package all-the-icons-dired
  :disabled
  :if (display-graphic-p)
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package ansible
  :defer)

(use-package anzu
  :disabled
  :config
  (global-anzu-mode))

(use-package avy
  :defer
  :custom
  (avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o)) ;; Colemak specific
  (avy-case-fold-search nil)
  :init
  (evil-leader/set-key "f" 'evil-avy-goto-char)
  (evil-leader/set-key "#" 'evil-avy-goto-line)
  (evil-leader/set-key "F" 'evil-avy-goto-word-or-subword-1)
  (evil-leader/set-key "/" 'avy-goto-char-timer)
  (evil-declare-not-repeat 'avy-goto-char-timer)
  (avy-setup-default))

(use-package bookmark+
  :disabled
  :straight (bookmark-plus :type git :host github
                           :repo "emacsmirror/bookmark-plus")
  :defer
  :config
  (bmkp-info-auto-bookmark-mode))

(use-package buffer-move
  :if (not (eq system-type 'windows-nt))
  :bind (("C-S-<up>"    . buf-move-up)
         ("C-S-<down>"  . buf-move-down)
         ("C-S-<left>"  . buf-move-left)
         ("C-S-<right>" . buf-move-right)))

(use-package crystal-mode
  :defer)

(use-package csv-mode
  :defer)

(use-package cypher-mode
  :defer)

(use-package dash-functional
  :defer)

(use-package deft
  :defer
  :custom
  (deft-directory "~/org/roam")
  (deft-recursive t)
  :config
  (add-hook 'deft-mode-hook
            (lambda ()
              (display-line-numbers-mode -1))))

(use-package desktop
  :defer
  :config
  (push ".*" desktop-clear-preserve-buffers))

(use-package dhall-mode
  :defer
  :custom
  (dhall-use-header-line nil)
  (dhall-format-at-save nil)
  :config
  ;; Note that a working LSP integration depends on `dhall-lsp-server' being
  ;; installed in your system. Preferably install this using:
  ;; `nix-env -i dhall-lsp-server'.
  (add-to-list 'lsp-language-id-configuration '(dhall-mode . "dhall"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "dhall-lsp-server")
                    :major-modes '(dhall-mode)
                    :server-id 'dhall-lsp-server))

  (add-hook 'dhall-mode-hook
            (lambda ()
              (lsp-deferred)

              ;; This is necessary to auto-insert matching " in `dhall-mode'.
              (sp-local-pair 'dhall-mode "\"" "\"" :actions '(add))

              ;; Do not treat "-" as a word separator.
              (modify-syntax-entry ?- "w"))))

(use-package dired+
  :straight (dired-plus :type git :host github :repo "emacsmirror/dired-plus")
  :after dired
  :custom
  (dired-dwim-target t)
  (diredp-hide-details-initially-flag nil)
  :config
  (diredp-toggle-find-file-reuse-dir 1)
  (define-key dired-mode-map "\C-w" 'evil-window-map)
  (add-hook 'dired-mode-hook (lambda () (evil-matchit-mode -1))))

(use-package diredful
  :disabled
  :after dired
  :config
  (diredful-mode))

(use-package direnv
  :defer
  :config
  (direnv-mode))

(defun mydired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first, before adding marks."
  (mydired-sort))

(use-package dired-subtree
  :after dired+
  :config
  (bind-keys :map dired-mode-map
    ("i" . dired-subtree-insert)
    (";" . dired-subtree-remove)))

(use-package docker-compose-mode
  :defer)

(use-package dockerfile-mode
  :defer)

(use-package elm-mode
  :defer
  :config
  (remove-hook 'elm-mode-hook 'elm-indent-mode)
  (add-hook 'elm-mode-hook #'elm-format-on-save-mode)
  (unbind-key (kbd "<tab>") 'elm-indent-simple-mode-map))

(use-package gcmh
  :config
  (gcmh-mode))

(use-package gif-screencast
  :defer
  :bind (:map gif-screencast-mode-map
         ("<f1>" . gif-screencast-stop)
         ("<f2>" . gif-screencast-toggle-pause))
  :config
  (setq gif-screencast-args '("--quality" "75" "--focused")))

(defmacro jco/set-eyebrowse-win-bindings ()
  "Generate evil-leader bindings for switching eyebrowse windows."
  `(progn ,@(mapcar
             (lambda (i)
               (let ((sym (intern
                           (format "eyebrowse-switch-to-window-config-%d" i))))
                 (evil-leader/set-key (format "w %d" i) sym)))
             (number-sequence 0 9))))

(use-package eyebrowse
  :defer 1
  :custom
  (eyebrowse-keymap-prefix "")
  (eyebrowse-mode-line-separator " ")
  (eyebrowse-new-workspace t)
  :init
  (evil-leader/set-key "w w" 'eyebrowse-switch-to-window-config)
  (evil-leader/set-key "w r" 'eyebrowse-rename-window-config)
  (jco/set-eyebrowse-win-bindings)
  (evil-leader/set-key "w c"
    (lambda ()
      (interactive)
      (when (projectile-project-root)
        (projectile-kill-buffers))
      (eyebrowse-close-window-config)))
  :config
  (eyebrowse-mode)
  (set-face-foreground 'mode-line-emphasis "#f9ca24"))

(use-package ggtags
  :if (eq system-type 'windows-nt)
  :defer)

(use-package ghub
  :defer)

(use-package elec-pair
  :defer 1
  :custom
  (electric-pair-skip-whitespace nil)
  (electric-pair-delete-adjacent-pairs nil)
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  :config
  (electric-pair-mode))

(use-package esup
  :defer
  :config
  (define-key esup-mode-map "\C-w" 'evil-window-map))

(use-package expand-region
  :bind
  (("C-=" . er/expand-region)))

(use-package f
  :defer)

(use-package fireplace
  :defer)

(use-package flycheck
  :defer
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq flycheck-pos-tip-timeout 0))

(use-package flycheck-package
  :after flycheck
  :config
  (flycheck-package-setup))

(use-package flycheck-pos-tip
  :after flycheck
  :config
  (flycheck-pos-tip-mode))

(use-package forge
  :after magit
  :config
  (add-hook 'forge-post-mode-hook
            (lambda ()
              (ethan-wspace-mode -1)
              (turn-off-auto-fill)
              (setq truncate-lines nil)
              (setq word-wrap t))))

(use-package fortune
  :if (not (eq system-type 'windows-nt))
  :disabled
  :config
  (setq fortune-dir "/usr/share/games/fortunes")
  (setq fortune-file "/usr/share/games/fortunes"))

(use-package fortune-cookie
  :if (not (eq system-type 'windows-nt))
  :disabled
  :config
  (setq fortune-cookie-cowsay-enable t)
  (setq fortune-cookie-cowsay-args "-f tux")
  (fortune-cookie-mode))

(use-package git-gutter+
  :disabled
  :if (not (eq system-type 'windows-nt))
  :config
  (global-git-gutter+-mode))

(use-package git-gutter-fringe+
  :disabled
  :after nlinum
  :if (not (eq system-type 'windows-nt)))

(use-package git-link
  :after magit
  :config
  (setq git-link-open-in-browser t)
  (evil-leader/set-key "g h" 'git-link-homepage)
  (evil-leader/set-key "g l" 'git-link))

(use-package github-notifier
  :disabled
  :if (locate-file "google-chrome-stable" exec-path exec-suffixes 1)
  :defer 2
  :config
  (github-notifier-mode))

(use-package google-this
  :defer
  :init
  (evil-leader/set-key "x g" 'google-this))

(use-package help-fns+
  :defer)

;; To get colors in html export of org-mode code snippets.
(use-package htmlize
  :defer)

(use-package ix
  :defer)

(use-package kurecolor)

(use-package ledger-mode
  :defer
  :mode "\\.journal\\'"
  :config
  (setq ledger-mode-should-check-version nil)
  (setq ledger-report-links-in-register nil)
  (setq ledger-binary-path "hledger")
  (add-to-list 'ledger-reports
               `("monthly expenses"
                 ,(concat "%(binary) -f %(ledger-file) balance expenses"
                          " --tree --no-total --row-total --average --monthly"))
               t))

(use-package lorem-ipsum
  :defer)

(use-package lsp-mode
  :hook
  ((c-mode . lsp-deferred)
   (c++-mode . lsp-deferred)

   (clojure-mode . lsp-deferred)

   ;; Requires `gopls' binary.
   (go-mode . lsp-deferred)

   (elm-mode . lsp-deferred)
   (js-mode . lsp-deferred))

  :custom
  (lsp-lens-enable nil)

  :init
  (with-eval-after-load 'lsp-mode
    ;; To avoid watching all Scrive API docs.
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]api_docs\\'" t)
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]_build-adminonly\\'" t)
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]_local\\'" t)
    (evil-leader/set-key
      "l" lsp-command-map))
  (add-hook 'lsp-mode-hook
            (lambda ()
              (lsp-enable-which-key-integration)))

  :config
  ;; This is to make `lsp-mode' work with `direnv' and pick up the correct
  ;; version of GHC.
  (advice-add 'lsp :before #'direnv-update-environment)
  (setq lsp-modeline-code-actions-enable nil))

(use-package lsp-ui
  :hook (prog-mode . lsp-ui-mode)
  :config
  (evil-leader/set-key "x m" #'lsp-ui-imenu)
  (setq lsp-ui-doc-position 'bottom))

(defun jco/magit-kill-buffers ()
  "Restore window configuration and kill all Magit buffers."
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (mapc #'kill-buffer buffers)))

(use-package minions
  :config
  (minions-mode)
  (setq minions-mode-line-lighter "#"))

(use-package nginx-mode
  :defer)

(use-package ob-async
  :after org-mode)

(use-package org-gcal
  :disabled
  :after calfw
  :config
  (require 'my-secrets (concat user-emacs-directory "lisp/my-secrets.el.gpg"))
  (setq org-gcal-file-alist
        '(("jonas.collberg@zimpler.com" . "~/Sync/emacs/gcal_zimpler.org"))))

(use-package outline
  :defer
  :init
  (if (version< emacs-version "25.1")
      (add-hook 'ediff-prepare-buffer-hook #'show-all)
    (add-hook 'ediff-prepare-buffer-hook #'outline-show-all)))

(use-package package-build
  :defer)

(use-package package-utils
  :defer)

(use-package pdf-tools
  :if (and (not (eq system-type 'windows-nt))
           (display-graphic-p))
  :defer 1
  :init
  (pdf-tools-install)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  ;; (setq pdf-annot-activate-created-annotations t)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (display-line-numbers-mode -1))))

(use-package rainbow-mode
  :defer)

(use-package rainbow-delimiters
  :defer)

(use-package recentf
  :defer
  :config
  (add-to-list 'recentf-exclude "/\\.emacs\\.d/elpa/")
  (add-to-list 'recentf-exclude "/\\.elfeed/index")
  (recentf-mode)
  (setq recentf-max-menu-items 25))

(use-package restclient
  :defer
  :config
  (dolist (mode-map '((html-mode . motion)
                      (js-mode   . motion)))
    (evil-set-initial-state (car mode-map) (cdr mode-map))))

(defun sp--org-skip-markup (ms mb me)
  (save-excursion
    (and (progn
           (goto-char mb)
           (save-match-data (looking-back "\\sw\\|\\s_\\|\\s.")))
         (progn
           (goto-char me)
           (save-match-data (looking-at "\\sw\\|\\s_\\|\\s."))))))

(use-package smartparens
  ;; :hook (prog-mode text-mode cider-repl-mode)
  :defer
  :config
  (sp-use-paredit-bindings)
  ;; (sp-pair "\"" nil :actions :rem)
  (show-smartparens-global-mode)
  (setq sp-navigate-interactive-always-progress-point t)
  (jco/define-bindings global-map
                       '(("M-(" . (lambda (&optional arg)
                                    (interactive "P")
                                    (sp-wrap-with-pair "(")))
                         ("M-{" . (lambda (&optional arg)
                                    (interactive "P")
                                    (sp-wrap-with-pair "{")))
                         ("M-\"" . (lambda (&optional arg)
                                     (interactive "P")
                                     (sp-wrap-with-pair "\"")))))
  ;; This is needed to avoid problems when using tmux in console mode.
  ;; Before this, things would become wrapped in brackets when switching
  ;; panes.
  (when (display-graphic-p)
    (jco/define-bindings global-map
                         '(("M-[" . (lambda (&optional arg)
                                      (interactive "P")
                                      (sp-wrap-with-pair "["))))))
  (jco/define-bindings smartparens-mode-map
                       '(("M-?" . sp-convolute-sexp)
                         ("C-k" . sp-kill-hybrid-sexp)
                         ("M-j" . sp-join-sexp)
                         ("M-C" . sp-clone-sexp)
                         ("C-M-n" . sp-next-sexp)
                         ("C-M-p" . sp-previous-sexp)
                         ("C-M-e" . sp-up-sexp)
                         ("C-M-d" . sp-down-sexp)
                         ("C-M-a" . sp-backward-down-sexp)
                         ("C-S-d" . sp-beginning-of-sexp)
                         ("C-S-a" . sp-end-of-sexp)))
  (sp-with-modes (cl-set-difference sp-lisp-modes sp-clojure-modes)
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p))
    (sp-local-pair "`" nil
                   :skip-match
                   (lambda (ms mb me)
                     (cond
                      ((equal ms "'")
                       (or (sp--org-skip-markup ms mb me)
                           (not (sp-point-in-string-or-comment))))
                      (t (not (sp-point-in-string-or-comment)))))))
  (sp-with-modes sp-clojure-modes
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" nil :actions nil))
  (evil-leader/set-key ")" 'sp-forward-slurp-sexp)
  (evil-leader/set-key "(" 'sp-backward-slurp-sexp)
  (evil-leader/set-key "}" 'sp-forward-barf-sexp)
  (evil-leader/set-key "{" 'sp-backward-barf-sexp))

(defun jco/camel-case-to-sentence (text)
  "Convert TEXT from camelCase to a sentence.
Example: `helloWorld` becomes `Hello world`."
  (interactive)
  (let* ((snake (string-inflection-underscore-function text))
         (words (replace-regexp-in-string "_" " " snake)))
    (jco/capitalize-first-char words)))

(defun jco/cpp-class-name ()
  "Return the class name corresponding to the name of the current buffer."
  (interactive)
  (let* ((base-name (file-name-base buffer-file-name)))
    (string-inflection-camelcase-function base-name)))

(use-package super-save
  :defer 1
  :config
  (setq super-save-exclude '(".gpg" ".pdf"))
  (super-save-mode))

(use-package unkillable-scratch
  :defer 1
  :init
  (unkillable-scratch))

(use-package volatile-highlights
  :defer 1
  :config
  (volatile-highlights-mode))

(use-package yaml-mode
  :mode "\\.yml\\'"
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (modify-syntax-entry ?- "w")
              (setq evil-shift-width 2))))

(require 'server)

(use-package server
  :if (and (not (server-running-p))
           (not (daemonp)))
  :defer 1
  :config
  (server-start)
  (require 'org-protocol))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :defer
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (exec-path-from-shell-initialize))))

(setq large-file-warning-threshold nil)

(setq safe-local-variable-values
      '((cider-ns-refresh-after-fn . "integrant.repl/resume")
        (cider-ns-refresh-before-fn . "integrant.repl/suspend")
        (org-archive-location . "::* Archived Tasks")))

(put 'erase-buffer 'disabled nil)

(windmove-default-keybindings)

(require 'ibuffer)

(dolist (map (list ibuffer-mode-map))
  (define-key map "\C-w" 'evil-window-map))

(define-key jco/my-keys-mode-map (kbd "C-x b") 'ibuffer)

(use-package info+
  :disabled
  :straight (info-plus :type git :host github :repo "emacsmirror/info-plus")
  :after info)

(use-package s
  :defer)

(use-package tree-sitter
  :defer 1
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package wgrep
  :defer)

(use-package xterm-color
  :after magit-delta)

(use-package yagist
  :defer
  :config
  (setq yagist-view-gist t))

(defun create-scm-string (type branch)
  "Create a string to be shown in prompt.
TYPE is either \"git\" or \"hg\" and BRANCH is the branch name."
  (propertize (concat "[" type ":"
                      (if (not (string-empty-p branch))
                          branch
                        "no branch")
                      "] ")
              'face `(:foreground "#f62459")))

(defun get-scm-branch (dir)
  "Return Git or Mercurial branch name of directory DIR."
  (interactive)
  (cond ((and (eshell-search-path "git")
              (locate-dominating-file dir ".git"))
         (let* ((git-output
                 (shell-command-to-string
                  (concat "git branch | grep '\\*' | sed -e 's/^\\* //'")))
                (git-branch (if (not (string-empty-p git-output))
                                (substring git-output 0 -1)
                              "")))
           (create-scm-string "git" git-branch)))
        ((and (eshell-search-path "hg")
              (locate-dominating-file dir ".hg"))
         (let* ((hg-output
                 (shell-command-to-string (concat "hg branch")))
                (hg-branch (if (not (string-empty-p hg-output))
                               (substring hg-output 0 -1)
                             "")))
           (create-scm-string "hg" hg-branch)))
        (t "")))

(setq eshell-prompt-function
      (lambda ()
        (concat (get-scm-branch (eshell/pwd))
                (abbreviate-file-name (eshell/pwd)) "\n$ ")))

(setq eshell-highlight-prompt t
      eshell-prompt-regexp "\$ ")

(add-hook 'eshell-mode-hook
          (lambda ()
            (set-face-foreground 'eshell-prompt "#f39c12")
            (defalias 'ff 'find-file)
            (defalias 'open 'find-file)

            (define-key eshell-mode-map "\C-w" 'evil-window-map)))

(defun jco/eshell-here ()
  "Open up a new shell in the directory associated with the current buffer.
The eshell buffer is renamed to match that directory to make multiple eshell
windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(defun eshell/x ()
  "Quit eshell and delete its window."
  (eshell-quit-process)
  (when (window-parent)
    (delete-window)))

(cond
 ((and (eq system-type 'windows-nt) (display-graphic-p))
  (add-to-list 'default-frame-alist
               '(font . "Hack-10"))
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 100 60))

 ((and (eq system-type 'gnu/linux) (display-graphic-p))
  (if (>= (x-display-pixel-height) 2160)
      (add-to-list 'default-frame-alist
                   '(font . "FiraCodeMedium-22"))
    (add-to-list 'default-frame-alist
                 '(font . "FiraCodeMedium-11")))
  ;; (set-frame-size (selected-frame) 93 64)
  )

 ((eq system-type 'darwin)
  (setq mac-right-option-modifier 'none)
  (when (display-graphic-p)
    (if (<= (x-display-pixel-height) 900)
        (set-frame-size (selected-frame) 93 47)
      (set-frame-size (selected-frame) 93 60))
    (set-face-attribute 'default nil :height 145))))

(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
  :defer
  :config
  ;; Enable the "www" ligature in every possible major mode.
  (ligature-set-ligatures 't '("www"))

  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it.
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))

  ;; Source: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions#using-ligatureel
  ;; Enable ligatures in programming modes.
  (ligature-set-ligatures
   '(clojure-mode dhall-mode haskell-mode)
   '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
     ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
     "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
     "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" "/*" "/**"
     "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
     "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
     "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
     "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
     "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
     "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

  (global-ligature-mode t))

(use-package hydra
  :defer
  :init
  (evil-leader/set-key "m" 'jco/hydra-main-menu/body))

(defun open-config-file (file-name)
  "Open FILE-NAME in emacs configuration directory."
  (interactive)
  (find-file (concat user-emacs-directory file-name)))

(defhydra jco/hydra-main-menu (:color teal :hint nil)
  "main menu"
  ("a" jco/hydra-apps/body "apps")
  ("b" counsel-bookmark "bookmarks")
  ("c" jco/hydra-config/body "cfg")
  ("f" jco/hydra-find/body "find")
  ("g" jco/hydra-gtd/body "gtd")
  ("h" jco/hydra-hideshow/body "hideshow")
  ("l" jco/hydra-lang/body "lang")
  ("o" jco/hydra-org/body "org")
  ("s" jco/hydra-consult/body "consult")
  ("u" jco/hydra-util/body "util")
  ("w" jco/hydra-writing/body "writing"))

(defhydra jco/hydra-config (:color teal :hint nil)
  "config"
  ("e" (find-file "~/dotfiles/homedir/.emacs.d/init.org") "edit")
  ("u" jco/update-dotfiles "update"))

(defhydra jco/hydra-find (:color teal :hint nil)
  "
find: _f_un _l_ib _v_ar"
  ("f" find-function)
  ("l" find-library)
  ("v" find-variable))

(defhydra jco/hydra-gtd (:color teal :hint nil)
  "gtd"
  ("b" (jco/find-org-file "blog.org") "blog")
  ("h" (jco/find-org-file "health.org") "health")
  ("i" (jco/find-org-file "incoming.org") "incoming")
  ("n" (jco/find-org-file "notes.org") "notes")
  ("p" (jco/find-org-file "todo.org" (projectile-project-root)) "project-todo")
  ("R" (jco/find-org-file "reading.org") "reading")
  ("r" (jco/find-org-file "retro.org") "retro")
  ("s" (jco/find-org-file "standup.org") "standup")
  ("t" (jco/find-org-file "todo.org") "todo")
  ("w" (jco/find-org-file "work.org") "work"))

(defvar jco/global-hl-line-mode-hydra-temp)
(set (make-local-variable 'jco/global-hl-line-mode-hydra-temp) nil)

(defhydra jco/hydra-hideshow (:color teal :hint nil)
  "hideshow"
  ("a" hs-show-all "show-all")
  ("t" hs-hide-all "hide-all")
  ("c" hs-toggle-hiding "toggle-hiding")
  ("d" hs-hide-block "hide-block")
  ("s" hs-show-block "show-block"))

(defhydra jco/hydra-lang (:color teal :hint nil)
  "
lang: _f_lyspell _l_angtool _c_orrect _d_one _s_dcv"
  ("f" flyspell-mode)
  ("l" langtool-check)
  ("c" langtool-correct-buffer)
  ("d" langtool-check-done)
  ("s" sdcv-search))

(defhydra jco/hydra-org (:color teal :hint nil)
  "org"
  ("a" org-agenda-list "agenda")
  ("c" org-clock-goto "org-clock-goto")
  ("d" deft "deft")
  ("g" org-capture-goto-last-stored "goto captured")
  ("p" org-pomodoro "org-pomodoro")
  ("x" org-clock-remove-overlays "remove clock overlays")
  ("G" org-refile-goto-last-stored "goto refiled")
  ("i" org-roam-insert "insert")
  ("f" org-roam-node-find "node-find")
  ("b" org-roam-buffer-activate "org-roam-buffer")
  ("t" org-roam-tag-add "add tag"))

(defhydra jco/hydra-consult (:color teal :hint nil)
  "org"
  ("a" consult-line-multi "consult-line-multi"))

(defhydra jco/hydra-text (:color teal :hint nil)
  "
text: _c_lean-trailing-ws"
  ("c" ethan-wspace-clean-all))

(defhydra jco/hydra-util (:color teal :hint nil)
  "
util: _k_urecolor _y_ank-filename insert-_f_ilename insert-_b_asename insert-_d_ate _e_diff-regions-wordwise ninsert-_t_imestamp _g_ist _h_ide-modeline _m_arkdown-other-window"
  ("k" jco/hydra-kurecolor/body)
  ("y" jco/yank-current-filename)
  ("f" jco/insert-current-filename)
  ("b" (lambda () (interactive) (jco/insert-current-filename t)))
  ("d" jco/insert-date)
  ("e" ediff-regions-wordwise)
  ("t" jco/insert-timestamp)
  ("g" yagist-region-or-buffer)
  ("h" hide-mode-line-mode)
  ("m" (lambda ()
         (interactive)
         (markdown-other-window)
         (browse-url-of-buffer markdown-output-buffer-name))))

(defhydra jco/hydra-kurecolor
  (:color pink :hint nil
   :pre (progn (set 'jco/global-hl-line-mode-hydra-temp (global-hl-line-mode))
               (global-hl-line-mode -1))
   :post (global-hl-line-mode jco/global-hl-line-mode-hydra-temp))
  "
kurecolor: _H_ue(+) _h_ue(-) _S_aturation(+) _s_aturation(-) _B_rightness(+) _b_rightness(-)"
  ("H" kurecolor-increase-hue-by-step)
  ("h" kurecolor-decrease-hue-by-step)
  ("S" kurecolor-increase-saturation-by-step)
  ("s" kurecolor-decrease-saturation-by-step)
  ("B" kurecolor-increase-brightness-by-step)
  ("b" kurecolor-decrease-brightness-by-step)
  ("q" nil "quit" :color blue))

(defhydra jco/hydra-writing (:color teal :hint nil)
  "writing"
  ("d" darkroom-tentative-mode "darkroom-tentative")
  ("D" darkroom-mode "darkroom")
  ("l" ligature-mode "ligatures")
  ("n" org-noter "org-noter")
  ("o" (jco/toggle-mode olivetti-mode) "olivetti"))

(defhydra jco/hydra-apps (:color teal :hint nil)
  "app"
  ("c" cfw:open-org-calendar "calendar")
  ("e" (erc :server "irc.freenode.net" :port 6667) "erc")
  ("f" (jco/elfeed-load-db-and-open) "elfeed")
  ("m" (lambda ()
         (interactive)
         (jco/init-mu4e-contexts)
         (require 'mu4e)
         (mu4e)) "mu4e")
  ("s" jco/eshell-here "eshell")
  ("v" jco/vim "vim")
  ("w" eww "eww")
  ("x" sx-tab-all-questions "sx"))

(defhydra jco/hydra-apropos (:color teal :hint nil)
  "
apropos: _a_propos _c_md _d_oc _v_al _l_ib _o_ption _v_ar _i_nfo _x_ref-find"
  ("a" apropos)
  ("c" apropos-command)
  ("d" apropos-documentation)
  ("e" apropos-value)
  ("l" apropos-library)
  ("o" apropos-user-option)
  ("v" apropos-variable)
  ("i" info-apropos)
  ("x" xref-find-apropos))

(use-package flyspell-correct
  :bind (:map flyspell-mode-map
         (("C-;" . flyspell-correct-wrapper))))

(use-package langtool
  :defer
  :init
  (setq langtool-language-tool-jar
        "/opt/LanguageTool-3.9/languagetool-commandline.jar")
  (setq langtool-autoshow-message-function 'langtool-autoshow-detail-popup))

(when (eq system-type 'darwin)
  (setenv "STARDICT_DATA_DIR" (expand-file-name "~/dictionaries")))

(defun langtool-autoshow-detail-popup (overlays)
  (when (require 'popup nil t)
    ;; Do not interrupt current popup.
    (unless (or popup-instances
                ;; Suppress popup after typing `C-g` .
                (memq last-command '(keyboard-quit)))
      (let ((msg (langtool-details-error-message overlays)))
        (popup-tip msg)))))

(use-package sdcv
  :defer
  :init
  (global-set-key (kbd "C-c d") 'sdcv-search-input))

(fset 'jco/paste-over [?\" ?0 ?p])

(fset 'jco/paste-over-word [?v ?i ?w ?\" ?0 ?p])

(evil-leader/set-key "p" 'jco/paste-over)
(evil-leader/set-key "P" 'jco/paste-over-word)

(use-package magit
  :defer 1
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :init
  (let ((filename "~/.nix-profile/bin/emacsclient"))
    (when (file-exists-p filename)
      (setq-default with-editor-emacsclient-executable
                    (expand-file-name filename))))
  :config
  ;; Fix regression where error message is shown when using magit-status while
  ;; having global-whitespace-mode enabled.
  (add-function :before-while whitespace-enable-predicate
    'prevent-whitespace-mode-for-magit)

  ;; Needed for success status message to be shown.
  (setq magit-auto-revert-mode nil)

  (evil-leader/set-key "v l" 'magit-log-buffer-file)
  (evil-leader/set-key "v b" 'magit-blame)
  (bind-key "q" #'jco/magit-kill-buffers magit-status-mode-map)
  (unless (display-graphic-p)
    (remove-hook 'magit-section-highlight-hook 'magit-section-highlight)
    (remove-hook 'magit-section-highlight-hook 'magit-diff-highlight))
  (add-hook 'git-commit-setup-hook
            (lambda ()
              (setq fill-column 72)
              (auto-fill-mode)
              (modify-syntax-entry ?- "w")
              (git-commit-turn-on-flyspell)
              (end-of-line)))
  (setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
  (add-hook 'magit-mode-hook
            (lambda ()
              (evil-local-set-key 'normal (kbd "SPC")
                                  'magit-diff-show-or-scroll-up)))
  (setq magit-clone-name-alist
        (map-insert magit-clone-name-alist
                    "\\`\\(?:bitbucket:\\|bb:\\)\\([^:]+\\)\\'"
                    '("bitbucket.org" "bitbucket.user"))))

(use-package magit-delta
  :if (locate-file "delta" exec-path exec-suffixes 1)
  :straight (magit-delta :type git :host github
                         :repo "dandavison/magit-delta")
  :after magit
  :config
  (when (< (kurecolor-hex-get-brightness (face-attribute 'default :background))
           0.5)
    (setq magit-delta-delta-args
          `("--plus-color" "#016000"
            "--plus-emph-color" "#02a000"
            "--minus-color" "#840001"
            "--minus-emph-color" "#b60004"
            "--max-line-distance" "0.6"
            "--24-bit-color" ,(if xterm-color--support-truecolor
                                  "always"
                                "never")
            "--color-only")))
  (magit-delta-mode))

(use-package magit-org-todos
  :disabled
  :after magit
  :config
  (magit-org-todos-autoinsert))

(use-package rich-minority
  :defer
  :config
  (setq rm-blacklist ".")
  (rich-minority-mode))

(use-package smart-mode-line
  :disabled
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

(use-package doom-modeline
  :disabled
  :ensure t
  :defer
  :hook (after-init . doom-modeline-init))

(require 's)

(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

(setq gnus-select-method
      '(nnimap "gmail.com"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))

(setq smtpmail-smtp-service 587)
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq send-mail-function    'smtpmail-send-it
      smtpmail-smtp-server  "smtp.gmail.com"
      smtpmail-stream-type  'starttls
      smtpmail-smtp-service 587)

(defun jco/init-mu4e-contexts ()
  "Initialize mu4e contexts."
  (require 'mu4e-context)
  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "Gmail"
            :enter-func (lambda ()
                          (mu4e-message "Switch to the Gmail context"))
            ;; leave-func not defined
            :match-func (lambda (msg)
                          (if msg
                              (mu4e-message-contact-field-matches
                               msg :to "jonas.collberg@gmail.com")
                            (not (jco/at-office-p))))
            :vars '((user-mail-address . "jonas.collberg@gmail.com")
                    (smtpmail-smtp-user . "jonas.collberg@gmail.com")
                    ;; (mu4e-compose-signature . "Jonas\n")
                    (mu4e-drafts-folder . "/gmail/Drafts")
                    (mu4e-sent-folder . "/gmail/Sent")
                    (mu4e-trash-folder . "/gmail/Trash")
                    (mu4e-maildir-shortcuts . (("/gmail/Inbox" . ?i)
                                               ("/gmail/Sent" . ?s)
                                               ("/gmail/Trash" . ?t)))
                    (mu4e-completing-read-function . jco/compl-fun)))

          ,(make-mu4e-context
            :name "Work"
            :enter-func (lambda () (mu4e-message "Switch to the Work context"))
            ;; leave-fun not defined
            :match-func (lambda (msg)
                          (if msg
                              (mu4e-message-contact-field-matches
                               msg :to "jonas.collberg@scrive.com")
                            (jco/at-office-p)))
            :vars '((user-mail-address . "jonas.collberg@scrive.com")
                    (smtpmail-smtp-user . "jonas.collberg@scrive.com")
                    ;; (mu4e-compose-signature . (concat
                    ;;                             "Kind regards,\n"
                    ;;                             user-full-name))
                    (mu4e-drafts-folder . "/scrive/[Gmail].Drafts")
                    (mu4e-sent-folder . "/scrive/[Gmail].Sent Mail")
                    (mu4e-trash-folder . "/scrive/[Gmail].Trash")
                    (mu4e-maildir-shortcuts .
                                            (("/scrive/Inbox" . ?i)
                                             ("/scrive/[Gmail].Sent Mail" . ?s)
                                             ("/scrive/[Gmail].Trash" . ?t)
                                             ("/scrive/[Gmail].All Mail" . ?a)))
                    (mu4e-completing-read-function . jco/compl-fun))))))

(when (and (not (eq system-type 'windows-nt))
           (not (string-equal (system-name) "jco")))

  (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

  (add-hook 'mu4e-main-mode-hook
            (lambda ()
              (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
              (require 'mu4e)
              (require 'mu4e-contrib)
              (require 'imapfilter)
              (setq mu4e-maildir "~/.mail")
              (dolist (m (list mu4e-main-mode-map
                               mu4e-headers-mode-map
                               mu4e-view-mode-map))
                (define-key m "\C-w" 'evil-window-map))
              (dolist (h (list 'mu4e-main-mode-hook
                               'mu4e-headers-mode-hook
                               'mu4e-view-mode-hook))
                (add-hook h (lambda () (evil-matchit-mode -1))))
              (setq mu4e-get-mail-command "mbsync -a")
              (setq mu4e-update-interval nil)
              (setq mu4e-sent-messages-behavior 'sent)
              (setq mu4e-html2text-command 'mu4e-shr2text)
              (setq shr-color-visible-luminance-min 60)
              (setq shr-color-visible-distance-min 5)
              (setq shr-use-colors nil)
              (advice-add #'shr-colorize-region :around (defun shr-no-colourise-region (&rest ignore)))
              (setq mu4e-view-show-images t)
              (setq mu4e-view-show-addresses t)
              (add-to-list 'mu4e-view-actions '("ViewInBrowser" .
                                                mu4e-action-view-in-browser) t)
              (setq mu4e-view-show-addresses t)
              (setq mu4e-compose-context-policy 'always-ask)
              (setq mu4e-compose-in-new-frame t)
              (setq mu4e-save-multiple-attachments-without-asking t)
              (setq mu4e-compose-format-flowed t)
              (setq mu4e-compose-dont-reply-to-self t)
              (setq mu4e-headers-date-format "%Y-%m-%d %H:%M")
              (setq mu4e-headers-fields
                    '((:date    . 25)
                      (:flags   .  6)
                      (:from    . 22)
                      (:subject . nil)))

              (setq mu4e-view-fields '(:from :to :cc :bcc :subject :flags
                                       :date :maildir :mailing-list :tags
                                       :attachments :signature :decryption))

              (defun jco/compl-fun (prompt maildirs predicate require-match
                                           initial-input)
                (helm-comp-read prompt maildirs
                                :name prompt
                                :must-match t))

              (defun jco/smtp-server ()
                (cond ((or (s-contains? "gmail.com" user-mail-address)
                           (s-contains? "scrive.com" user-mail-address))
                       "smtp.gmail.com")))

              (defun jco/my-send-it ()
                (setq smtpmail-starttls-credentials
                      `((,(jco/smtp-server) 587 nil nil))
                      smtpmail-auth-credentials
                      `((,(jco/smtp-server) 587 user-mail-address nil))
                      smtpmail-default-smtp-server (jco/smtp-server)
                      smtpmail-smtp-server (jco/smtp-server))
                (smtpmail-send-it))

              (require 'smtpmail)

              (setq message-send-mail-function 'jco/my-send-it
                    starttls-use-gnutls t
                    smtpmail-smtp-service 587)

              ;; don't keep message buffers around
              (setq message-kill-buffer-on-exit t)

              (setq mu4e-org-contacts-file "~/.contacts")
              (add-to-list 'mu4e-headers-actions
                           '("org-contact-add" . mu4e-action-add-org-contact) t)
              (add-to-list 'mu4e-view-actions
                           '("org-contact-add" . mu4e-action-add-org-contact) t))))

(add-hook 'mu4e-update-pre-hook
          #'imapfilter)

(add-hook 'mu4e-compose-mode-hook
          (lambda ()
            (ethan-wspace-mode -1)
            (turn-off-auto-fill)
            (footnote-mode)
            (setq truncate-lines nil)
            (setq word-wrap t)))

(setq org-directory "~/org")
(setq org-roam-v2-ack t)

(defun jco/org-inline-css-hook (exporter)
  "Fix colors of snippets when EXPORTER is 'html.
Insert custom inline css to automatically set the foreground and background of
code, to the current theme's colors."
  (when (eq exporter 'html)
    (let* ((my-pre-bg (face-background 'default))
           (my-pre-fg (face-foreground 'default)))
      (setq
       org-html-head-extra
       (concat
        org-html-head-extra
        (format (concat "<style type=\"text/css\">\n pre.src "
                        "{background-color: %s; color: %s;}</style>\n")
                my-pre-bg my-pre-fg))))))

(defun jco/org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.
PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun jco/org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(cl-defun jco/add-youtube-link-type (name &optional (url-params nil))
  "Add org link type for embedding YouTube links in org-mode."
  (let ((yt-iframe-format
         (concat "<iframe width=\"560\""
                 " height=\"315\""
                 " src=\"https://www.youtube.com/embed/%s?rel=0"
                 url-params
                 "\""
                 " frameborder=\"0\""
                 " allowfullscreen>%s</iframe>")))
    (org-add-link-type name
                       (lambda (handle)
                         (browse-url
                          (concat "https://www.youtube.com/embed/"
                                  handle)))
                       (lambda (path desc backend)
                         (cl-case backend
                           (html (format yt-iframe-format
                                         path (or desc "")))
                           (latex (format "\href{%s}{%s}"
                                          path
                                          (or desc "video"))))))))

;; See: https://github.com/emacs-evil/evil-surround/issues/20#issuecomment-471516289
(defmacro define-and-bind-quoted-text-object (name key start-regex end-regex)
  (let ((inner-name (make-symbol (concat "evil-inner-" name)))
        (outer-name (make-symbol (concat "evil-a-" name))))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key #',inner-name)
       (define-key evil-outer-text-objects-map ,key #',outer-name))))

;; Custom link types

(defun my-org-link-make-description-function (link _desc)
  "Remove my custom prefixes from LINK."
  (when (string-prefix-p "jira:" link)
    (string-remove-prefix "jira:" link)))

(use-package cha
  :disabled
  :straight (cha :type git :host github :repo "joncol/cha")
  :commands (cha-create-story cha-edit-story)
  :init
  (evil-leader/set-key "x c" #'cha-create-story)
  (evil-leader/set-key "x e" #'cha-edit-story)
  :config
  (setq cha-clubhouse-default-project "Backend")
  (require 'my-secrets (concat user-emacs-directory "lisp/my-secrets.el.gpg")))

(use-package org-bullets
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

(use-package org-cliplink
  :defer
  :init
  (evil-leader/set-key-for-mode 'org-mode "x l" 'org-cliplink))

(use-package org-download
  :after org
  :init
  (add-hook 'org-mode-hook
            (lambda ()
              (require 'org-download))))

(use-package org-re-reveal
  :after org)

(defun jco/ensure-todo-org-header ()
  "If the current buffer is empty, insert an org header."
  (when (zerop (buffer-size))
    (insert (concat "#+SEQ_TODO: TODO(t) IN-PROGRESS(i) DONE(d)\n"
                    "#+STARTUP: showall\n\n"))))

(defun jco/goto-current-project-todo-org (headline)
  "Go to project's todo.org, section: HEADLINE."
  (set-buffer (org-capture-target-buffer (concat (projectile-project-root)
                                                 "todo.org")))
  (org-capture-put-target-region-and-position)
  (widen)
  (goto-char (point-min))
  (jco/ensure-todo-org-header)
  (if (re-search-forward (format org-complex-heading-regexp-format
                                 (regexp-quote headline))
                         nil t)
      (beginning-of-line)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "* " headline "\n")
    (beginning-of-line 0)))

(use-package org
  :defer
  :ensure org-plus-contrib
  :custom
  (org-footnote-auto-adjust t)
  (org-M-RET-may-split-line nil)
  (org-link-make-description-function #'my-org-link-make-description-function)
  :init
  (setq org-return-follows-link t)
  (setq org-startup-indented t)
  (setq org-edit-src-content-indentation 0)
  (setq org-capture-templates
        '(("t" "Task" entry (file+headline "incoming.org" "Incoming tasks")
           "* TODO %^{Description}\n:LOGBOOK:\n- Added: %U\n:END:\n%?\n"
           :empty-lines-before 0)
          ("p" "Project TODO" entry
           (function (lambda () (jco/goto-current-project-todo-org "Todos")))
           "* TODO %^{Description}\n:LOGBOOK:\n- Added: %U\n:END:\n%?\n"
           :empty-lines-before 0)
          ("n" "Note" entry (file+headline "notes.org" "Notes")
           "* %^{Description}\n:LOGBOOK:\n- Added: %U\n:END:\n%?\n"
           :empty-lines-before 0)
          ("a" "Appointment" entry (file "~/Sync/emacs/gcal_zimpler.org")
           "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
          ("w" "Web" entry (file+headline "web.org" "_Incoming")
           "* %:description\n%:initial\n\nSource: %:link\n:LOGBOOK:\n- Added: %U\n:END:\n"
           :empty-lines-before 0)
          ("s" "Standup entry" entry (file+headline "standup.org" "Entries")
           "* %U\n%?\n" :empty-lines-before 0)
          ("r" "Retrospective templates")
          ("rp" "Positive" entry (file+headline "retro.org" "Positives")
           "* TODO %^{Description}\n:LOGBOOK:\n- Added: %U\n:END:\n%?\n")
          ("rn" "Negative" entry (file+headline "retro.org" "Negatives")
           "* TODO %^{Description}\n:LOGBOOK:\n- Added: %U\n:END:\n%?\n")
          ("rl" "Learned" entry (file+headline "retro.org" "Learned")
           "* TODO %^{Description}\n:LOGBOOK:\n- Added: %U\n:END:\n%?\n")))
  :config
  (evil-leader/set-key-for-mode 'org-mode "z f" 'org-footnote-new)
  (add-hook 'org-capture-mode-hook
            (lambda ()
              (god-local-mode -1)))
  (setq org-startup-truncated nil)
  (setq org-src-fontify-natively t)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (setq org-log-done t)
  (setq org-default-notes-file "notes.org")
  (setq org-reveal-hlevel 2)
  (setq org-todo-keyword-faces
        '(("TODO" . "deep pink")
          ("IN-PROGRESS" . "orange")
          ("NEXT" . "green2")
          ("WAITING" . "purple")
          ("MAYBE" . "gray60")))
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.6))
  (setq org-agenda-files (concat org-directory "/agenda-files"))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 9)
                             ("~/org/notes.org" :maxlevel . 9)
                             ("~/org/reading.org" :maxlevel . 9)))
  (setq org-use-fast-todo-selection t)
  (setq org-log-into-drawer t)
  (setq org-enforce-todo-dependencies t)
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-agenda-custom-commands
        '(("d" "Daily agenda view"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header
                    "High-priority unfinished tasks:")))
            (agenda "" ((org-agenda-ndays 1)))
            (tags-todo "work"
                       ((org-agenda-skip-function
                         '(or (jco/org-skip-subtree-if-habit)
                              (jco/org-skip-subtree-if-priority ?A)
                              (org-agenda-skip-if nil '(scheduled deadline))))
                        (org-agenda-overriding-header
                         "All normal priority tasks, tagged with `work':"))))
           ((org-agenda-compact-blocks nil)))))
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (display-fill-column-indicator-mode -1)))
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-show-all-today t)
  (setq org-habit-show-habits-only-for-today t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)
     (dot . t)
     (haskell . t)
     (latex . t)
     (octave . t)
     (plantuml . t)
     (python . t)
     (shell . t)
     (sql . t)))
  (setq org-confirm-babel-evaluate nil)
  (if (eq system-type 'windows-nt)
      (setq org-ditaa-jar-path "c:/tools/misc/ditaa.jar"
            org-plantuml-jar-path "c:/tools/misc/plantuml.jar")
    (setq org-ditaa-jar-path "~/.nix-profile/lib/ditaa.jar"
          org-plantuml-jar-path "~/.nix-profile/lib/plantuml.jar"))
  (require 'ox-latex)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (add-to-list 'org-latex-inputenc-alist '("utf8" . "utf8x"))
  (setq org-latex-default-packages-alist
        (cons '("mathletters" "ucs" nil)
              org-latex-default-packages-alist))
  (setq org-latex-listings 'minted)
  (setq org-latex-custom-lang-environments
        '((emacs-lisp "common-lispcode")))
  (setq org-latex-minted-options
        '(("frame" "lines")
          ("fontsize" "\\normalsize")
          ;; ("fontsize" "\\scriptsize")
          ("mathescape" "")
          ("samepage" "")
          ("xrightmargin" "0.5cm")
          ("xleftmargin"  "0.5cm")))
  (setq org-latex-pdf-process
        '("latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f"))
  ;; (setq org-latex-pdf-process
  ;;       '("pdflatex -shell-escape -interaction=nonstopmode -output-directory=%o %f"
  ;;         "pdflatex -shell-escape -interaction=nonstopmode -output-directory=%o %f"
  ;;         "pdflatex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))
  (setq org-latex-table-caption-above nil)
  (setq org-latex-default-figure-position "!htb")
  (setq org-mobile-directory (concat org-directory "/mobile"))
  (setq org-mobile-inbox-for-pull (concat org-directory "/index.org"))
  (setq org-mobile-force-id-on-agenda-items nil)
  (global-unset-key (kbd "C-x C-v"))
  (jco/define-bindings org-mode-map
                       '(("<f5>" . (lambda ()
                                     (interactive)
                                     (org-remove-inline-images)
                                     (org-ctrl-c-ctrl-c)
                                     (org-display-inline-images)))))
  (require 'org-agenda)
  (bind-keys :map org-agenda-mode-map
    ("j"       . org-agenda-next-item)
    ("k"       . org-agenda-previous-item)
    ("C-w h"   . windmove-left)
    ("C-w j"   . windmove-down)
    ("C-w k"   . windmove-up)
    ("C-w l"   . windmove-right)
    ("C-w C-h" . windmove-left)
    ("C-w C-j" . windmove-down)
    ("C-w C-k" . windmove-up)
    ("C-w C-l" . windmove-right))
  (jco/add-youtube-link-type "yt")
  (jco/add-youtube-link-type "ytnc" "&controls=0")
  (define-key org-mode-map (kbd "M-o") 'ace-link-org)
  (setq org-hide-emphasis-markers t)
  (font-lock-add-keywords
   'org-mode
   '(("^ +\\([-*]\\) "
      (0 (prog1 () (compose-region (match-beginning 1)
                                   (match-end 1) "•"))))))
  (setq org-clock-persist 'history)

  (add-hook 'org-mode-hook
            (lambda ()
              (org-clock-persistence-insinuate)
              (turn-on-auto-fill)
              (display-fill-column-indicator-mode -1)
              (display-line-numbers-mode -1)
              ;; (flyspell-mode)
              (smartparens-mode -1)
              (evil-leader/set-key "z l" 'org-toggle-link-display)
              (setq company-idle-delay 0.5)
              (define-and-bind-quoted-text-object "tilde" "~" "~" "~")
              (define-and-bind-quoted-text-object "equals" "=" "=" "=")
              (define-and-bind-quoted-text-object "slash" "/" "/" "/")
              (define-and-bind-quoted-text-object "dollar" "$" "$" "$")
              (modify-syntax-entry ?- "w") ;; do not treat "_" as a word separator
              ))

  (add-hook 'org-export-before-processing-hook 'jco/org-inline-css-hook)
  (require 'ob-clojure)
  (eval-after-load "org"
    '(require 'ox-gfm nil t))
  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  (add-to-list 'org-latex-classes
               '("extarticle"
                 "\\documentclass[14pt]{extarticle}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; Set default column view headings: Task Total-Time Time-Stamp
  (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA"))

(use-package org-habit-plus
  :disabled
  :straight (org-habit-plus :type git :host github
                            :repo "oddious/org-habit-plus")
  :init
  (add-to-list 'org-modules 'org-habit))

;; Source: https://org-roam.discourse.group/t/creating-an-org-roam-note-from-an-existing-headline/978
(defun org-roam-create-note-from-headline ()
  "Create an Org-roam note from the current headline and jump to it.

Normally, insert the headline’s title using the ’#title:’ file-level property
and delete the Org-mode headline. However, if the current headline has a
Org-mode properties drawer already, keep the headline and don’t insert
‘#+title:'. Org-roam can extract the title from both kinds of notes, but using
‘#+title:’ is a bit cleaner for a short note, which Org-roam encourages."
  (interactive)
  (org-cycle 2)
  (let ((title (nth 4 (org-heading-components)))
        (has-properties (org-get-property-block)))
    (org-cut-subtree)
    (org-roam-find-file title nil nil 'no-confirm)
    (org-paste-subtree)
    (unless has-properties
      (kill-line)
      (while (outline-next-heading)
        (org-promote)))
    (goto-char (point-min))
    (when has-properties
      (kill-line)
      (kill-line))
    (goto-char (point-at-pos-rel-line-offset (point-min) 3))
    (delete-blank-lines)
    (indent-region (point-min) (point-max))))

(use-package org-noter
  :defer 1
  :if (and (not (eq system-type 'windows-nt))
           (display-graphic-p))
  :bind (:map org-noter-doc-mode-map
         (("M-I" . org-noter-insert-note)))
  :config
  (setq org-noter-always-create-frame nil)
  (setq org-noter-hide-other nil)
  (setq org-noter-notes-search-path '("~/org/roam"))
  (org-noter-set-auto-save-last-location t)
  (evil-leader/set-key "z n" 'org-noter))

(use-package org-noter-pdftools
  :after org-noter
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions
            (if toggle-no-questions
                (not org-noter-insert-note-no-questions)
              org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freestyle-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location
                      (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))

  ;; Don't focus PDF after syncing notes.

  (defun org-noter-sync-prev-note ()
    "Go to the location of the previous note, in relation to where the point is.
As such, it will only work when the notes window exists."
    (interactive)
    (org-noter--with-selected-notes-window
     "No notes window exists"
     (let ((org-noter--inhibit-location-change-handler t)
           (contents (org-element-contents (org-noter--parse-root)))
           (current-begin (org-element-property :begin (org-noter--get-containing-heading)))
           previous)
       (when current-begin
         (org-noter--map-ignore-headings-with-doc-file
          contents t
          (when location
            (if (= current-begin (org-element-property :begin headline))
                t
              (setq previous headline)
              nil))))

       (if previous
           (progn
             ;; NOTE(nox): This needs to be manual so we can focus the correct note
             (org-noter--doc-goto-location (org-noter--parse-location-property previous))
             (org-noter--focus-notes-region (org-noter--make-view-info-for-single-note session previous)))
         (user-error "There is no previous note")))))

  (defun org-noter-sync-current-note ()
    "Go the location of the selected note, in relation to where the point is.
As such, it will only work when the notes window exists."
    (interactive)
    (org-noter--with-selected-notes-window
     "No notes window exists"
     (if (string= (org-entry-get nil org-noter-property-doc-file t) (org-noter--session-property-text session))
         (let ((location (org-noter--parse-location-property (org-noter--get-containing-heading))))
           (if location
               (org-noter--doc-goto-location location)
             (user-error "No note selected")))
       (user-error "You are inside a different document"))))

  (defun org-noter-sync-next-note ()
    "Go to the location of the next note, in relation to where the point is.
As such, it will only work when the notes window exists."
    (interactive)
    (org-noter--with-selected-notes-window
     "No notes window exists"
     (let ((org-noter--inhibit-location-change-handler t)
           (contents (org-element-contents (org-noter--parse-root)))
           next)

       (org-noter--map-ignore-headings-with-doc-file
        contents t
        (when (and location (< (point) (org-element-property :begin headline)))
          (setq next headline)))

       (if next
           (progn
             (org-noter--doc-goto-location
              (org-noter--parse-location-property next))
             (org-noter--focus-notes-region
              (org-noter--make-view-info-for-single-note session next)))
         (user-error "There is no next note")))))

  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions
              #'org-noter-pdftools-jump-to-note)))

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-pomodoro
  :defer
  :custom
  (org-pomodoro-manual-break t)

  (org-pomodoro-start-sound-p t)
  (org-pomodoro-start-sound (concat user-emacs-directory
                                    "resources/sounds/ripples.wav"))
  (org-pomodoro-finished-sound (concat user-emacs-directory
                                       "resources/sounds/ripples.wav"))
  (org-pomodoro-overtime-sound (concat user-emacs-directory
                                       "resources/sounds/ripples.wav"))
  (org-pomodoro-short-break-sound (concat user-emacs-directory
                                          "resources/sounds/ripples.wav"))
  (org-pomodoro-long-break-sound (concat user-emacs-directory
                                         "resources/sounds/ripples.wav"))
  (org-pomodoro-killed-sound-p t)
  (org-pomodoro-killed-sound (concat user-emacs-directory
                                     "resources/sounds/clock.wav")))

(use-package org-roam
  :custom
  (org-roam-directory "~/org/roam")
  (org-roam-buffer-position 'bottom)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}
#+setupfile: ~/org/roam/template.org
#+created: %U
#+last_modified: %U")
      :unnarrowed t)
     ("p" "project" plain "%?"
      :if-new (file+head "projects/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}
#+setupfile: ~/org/roam/template.org
#+created: %U
#+last_modified: %U")
      :unnarrowed t)))
  :bind (:map org-roam-mode-map
         (("C-c n l" . org-roam)
          ("C-c n f" . org-roam-node-find)
          ("C-c n g" . org-roam-graph))
         :map org-mode-map
         (("C-c n i" . org-roam-insert))
         (("C-c n I" . org-roam-insert-immediate)))
  :init
  (add-hook 'after-init-hook
            (lambda ()
              (setq time-stamp-start "last_modified:[ ]+\\\\?")
              (setq time-stamp-end "$")
              (setq time-stamp-format "\[%Y-%02m-%02d %3a %02H:%02M\]")
              (setq time-stamp-line-limit 16)
              (add-hook 'before-save-hook #'time-stamp)))
  :config
  (org-roam-setup)
  (org-roam-bibtex-mode))

(use-package citeproc
  :defer)

(use-package org-roam-bibtex
  :after org-roam
  :config
  (require 'org-ref)
  (setq orb-preformat-keywords
        '("citekey" "title" "url" "file" "author-or-editor" "keywords"))
  (add-to-list 'org-roam-capture-templates
               '("l" "literature" plain "%?"
                 :target (file+head "literature/${citekey}.org"
                                    "#+title: ${title}
#+setupfile: ~/org/roam/template.org
#+roam_key: ${ref}
#+filetags: literature
#+created: %U
#+last_modified: %U

* ${title}\n:PROPERTIES:\n:custom_id: ${citekey}\n:url: ${url}
:author: ${author-or-editor}
:noter_document: %(orb-process-file-field \"${citekey}\")\n:noter_page:
:END:")
                 :unnarrowed t)))

(use-package ivy-bibtex
  :defer
  :init
  (evil-leader/set-key "z b" 'ivy-bibtex)
  :custom
  (ivy-bibtex-default-action 'ivy-bibtex-edit-notes)
  :config
  ;; Assumes usage of Zotero to export BibTeX bibliography.
  (setq bibtex-completion-bibliography '("~/Sync/Zotero/library.bib"))
  (setq bibtex-completion-pdf-field "File")
  (setq bibtex-completion-notes-path "~/org/roam/ref")
  (org-roam-bibtex-mode))

(use-package org-super-agenda
  :disabled
  :after org
  :custom
  (org-super-agenda-groups
   '(;; Each group has an implicit boolean OR operator between its selectors.
     (:name "Today"  ; Optionally specify section name
      :time-grid t  ; Items that appear on the time grid
      :todo "TODAY")  ; Items that have this TODO keyword
     (:name "Important"
      ;; Single arguments given alone
      :tag "bills"
      :priority "A")
     ;; Set order of multiple groups at once
     (:order-multi (2 (:name "Shopping in town"
                       ;; Boolean AND group matches items that match all subgroups
                       :and (:tag "shopping" :tag "@town"))
                      (:name "Food-related"
                       ;; Multiple args given in list with implicit OR
                       :tag ("food" "dinner"))
                      (:name "Personal"
                       :habit t
                       :tag "personal")
                      (:name "Space-related (non-moon-or-planet-related)"
                       ;; Regexps match case-insensitively on the entire entry
                       :and (:regexp ("space" "NASA")
                             ;; Boolean NOT also has implicit OR between selectors
                             :not (:regexp "moon" :tag "planet")))))
     ;; Groups supply their own section names when none are given
     (:todo "WAITING" :order 8)  ; Set order of this section
     (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
      ;; Show this group at the end of the agenda (since it has the
      ;; highest number). If you specified this group last, items
      ;; with these todo keywords that e.g. have priority A would be
      ;; displayed in that group instead, because items are grouped
      ;; out in the order the groups are listed.
      :order 9)
     (:priority<= "B"
      ;; Show this section after "Today" and "Important", because
      ;; their order is unspecified, defaulting to 0. Sections
      ;; are displayed lowest-number-first.
      :order 1)
     ;; After the last group, the agenda will display items that didn't
     ;; match any of these groups, with the default order position of 99
     ))
  :config
  (org-super-agenda-mode))

(use-package ox-hugo
  :after ox
  :init
  (with-eval-after-load 'org-capture
    (defun org-hugo-new-subtree-post-capture-template ()
      "Return `org-capture' template string for new Hugo post."
      (let* ((title (read-from-minibuffer "Post title: "))
             (filename (org-hugo-slug title)))
        (mapconcat #'identity
                   `(,(concat "* TODO " title)
                     ":PROPERTIES:"
                     ,(concat ":export_file_name: " filename)
                     ":END:"
                     "%?TODO: summary"
                     "#+hugo: more"
                     "TODO: content")
                   "\n")))

    (add-to-list 'org-capture-templates
                 '("h"
                   "Hugo post"
                   entry
                   (file+olp "blog.org" "Weblog ideas")
                   (function org-hugo-new-subtree-post-capture-template)))))

(jco/define-bindings global-map
                     '(("C-c a"   . org-agenda)
                       ("C-c c"   . org-capture)
                       ("C-c l"   . org-store-link)
                       ("C-c M-w" . org-copy)
                       ("C-c C-w" . org-refile)))

(use-package ox-slack
  :defer
  :init
  (evil-leader/set-key-for-mode 'org-mode "z s"
    'org-slack-export-to-clipboard-as-slack))

(defun jco/find-org-file (filename &optional dir)
  "Open file FILENAME in the directory DIR (default: `org-directory')."
  (find-file (concat (or dir org-directory) "/" filename))
  (jco/ensure-todo-org-header))

(use-package csharp-mode
  :mode "\\.cs\\'")

(global-set-key (kbd "C-c M-s") #'cider-selector)

(defun create-test-report-window (&rest _)
  "Create window to show test report buffer, if one exists.
Place it to the right of the current window. If a window for the test report
buffer already exists, don't create a new one."
  (when-let* ((buf (get-buffer cider-test-report-buffer)))
    (unless (get-buffer-window buf)
      (let ((buffer-window (split-window (selected-window)
                                         (/ (window-width) 2)
                                         'right)))
        (set-window-buffer buffer-window buf)
        (display-buffer-record-window 'window buffer-window buf)
        (set-window-prev-buffers buffer-window nil)
        (select-window buffer-window)))))

(use-package cider
  :defer
  :bind (:map clojure-mode-map
         ("M-." . cider-find-dwim))
  :config
  (advice-add 'cider-switch-to-repl-buffer :after #'jco/move-window-to-bottom)
  (advice-add 'cider-test-show-report :before #'create-test-report-window)
  (advice-add 'cider-popup-buffer :before #'create-test-report-window)
  (setq cider-repl-display-help-banner nil)
  (setq cider-show-error-buffer nil)
  (setq cider-auto-select-test-report-buffer t)
  (setq cider-test-show-report-on-success nil)
  (setq cider-jump-to-pop-to-buffer-actions
        '((display-buffer-reuse-window display-buffer-same-window)))
  ;; (setq cider-repl-result-prefix ";; => ")
  )

(use-package cider-eval-sexp-fu
  :after cider)

(use-package clj-refactor
  :after clojure-mode
  :custom
  (cljr-cljc-clojure-test-declaration
   "#?(:clj [clojure.test :refer [deftest is testing]]
:cljs [cljs.test :refer [deftest is testing] :include-macros true])")
  (cljr-cljs-clojure-test-declaration
   "[cljs.test :as [deftest is testing] :include-macros true]")
  (cljr-clojure-test-declaration
   "[clojure.test :refer [deftest is testing]]")
  :config
  (setq cljr-warn-on-eval nil)
  (setq cljr-auto-clean-ns nil)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode)
              (cljr-add-keybindings-with-prefix "C-c C-m")))

  (setq evil-motion-state-modes
        (append '(cider-docview-mode
                  cider-popup-buffer-mode
                  cider-inspector-mode
                  cider-classpath-mode)
                evil-motion-state-modes)))

(use-package flycheck-clj-kondo
  :defer)

(use-package kibit-helper
  :defer)

(defun nrepl-reset ()
  "Helper function to call the (Reloaded workflow) reset function."
  (interactive)
  (set-buffer (cider-current-repl))
  (goto-char (point-max))
  (insert "(reset)")
  (cider-repl-return))

(defun point-at-pos-rel-line-offset (pos rel-line-offset)
  "Return position of point at POS with REL-LINE-OFFSET relative line offset."
  (save-excursion
    (goto-char pos)
    (forward-line rel-line-offset)
    (point)))

(defun close-repl-window ()
  "Close the current REPL window."
  (cider-switch-to-repl-buffer)
  (delete-window))

(defun disassemble-clojure-fn ()
  "Helper function to disassemble a Clojure function.
Opens a new buffer with the result."
  (interactive)
  (let* ((fn-name  (read-string "Disassemble Clojure function: "
                                (thing-at-point 'symbol t)))
         (buf-name (concat fn-name "-disassembly")))
    (set-buffer (cider-current-repl-buffer))
    (goto-char (point-max))
    (insert "(use 'no.disassemble)")
    (cider-repl-return)
    (sleep-for 0 100)
    (goto-char (point-max))
    (insert (concat "(println (disassemble " fn-name "))"))
    (save-excursion
      (cider-repl-return))
    (sleep-for 0 100)
    (forward-line)
    (if (not (re-search-forward "CompilerException" (line-end-position) t))
        (progn (copy-to-buffer buf-name (point)
                               (point-at-pos-rel-line-offset (point-max) -1))
               (goto-char (point-max))
               (pop-to-buffer buf-name)
               (delete-trailing-whitespace)
               (java-mode))
      (progn
        (goto-char (point-max))
        (message (concat "No function named '" fn-name "' found"))))))

(defun cljfmt-buffer ()
  "Run `cljfmt --fix' on current buffer, after saving it."
  (interactive)
  (when (or (eq major-mode 'clojure-mode)
            (eq major-mode 'clojurescript-mode))
    (save-buffer)
    (shell-command-to-string (format "cljfmt --fix %s" buffer-file-name))
    (revert-buffer :ignore-auto :noconfirm)))

(add-hook 'clojure-mode-hook
          (lambda ()
            (setq lsp-ui-sideline-show-code-actions nil)

            (init-lisp-common)

            ;; This applies when using `fill-paragraph' (`M-q') with the point
            ;; being inside the docstring.
            (setq clojure-docstring-fill-column 69)

            (setq-local evil-move-beyond-eol t)
            (setq cider-prompt-for-symbol nil)

            (modify-syntax-entries)

            (cider-auto-test-mode)

            (require 'flycheck-clj-kondo)

            (define-key clojure-mode-map (kbd "M-;") #'jco/lisp-comment-dwim)

            (put-clojure-indent 'ANY 2)
            (put-clojure-indent 'GET 2)
            (put-clojure-indent 'POST 2)
            (put-clojure-indent 'PUT 2)
            (put-clojure-indent 'DELETE 2)
            (put-clojure-indent 'defstate nil)
            (put-clojure-indent 'try* 0)

            ;; Indentation for re-frame
            (put-clojure-indent 'reg-cofx 0)
            (put-clojure-indent 'reg-event-ctx 0)
            (put-clojure-indent 'reg-event-db 0)
            (put-clojure-indent 'reg-event-fx 0)
            (put-clojure-indent 'reg-fx 0)
            (put-clojure-indent 'reg-sub 0)
            (put-clojure-indent 'reg-sub-raw 0)
            (put-clojure-indent '->interceptor 0)
            (put-clojure-indent 'fn-traced 1)

            (put-clojure-indent 'extend-freeze 2)
            (put-clojure-indent 'extend-thaw 1)

            ;; Indentation for duct
            (put-clojure-indent 'context 2)

            (put-clojure-indent 'wcar 1)

            (put-clojure-indent 'alet 'defun)
            (put-clojure-indent 'mlet 'defun)

            (add-to-list 'clojure-align-binding-forms "m/mlet")
            (add-to-list 'clojure-align-binding-forms "m/alet")
            (add-to-list 'clojure-align-binding-forms "with-disposable")

            (put-clojure-indent 'in-terminal 1)))

(add-hook 'nrepl-connected-hook
          (lambda ()
            (jco/move-window-to-bottom)))

(add-hook 'cider-browse-ns-mode-hook
          (lambda ()
            ;; For some reason, `windmove-default-keybindings' doesn't work.
            (bind-window-keys cider-browse-ns-mode-map)))

(add-hook 'cider-stacktrace-mode-hook
          (lambda ()
            ;; For some reason, `windmove-default-keybindings' doesn't work.
            (bind-window-keys cider-stacktrace-mode-map)))

(add-hook 'cider-test-report-mode-hook
          (lambda ()
            ;; For some reason, `windmove-default-keybindings' doesn't work.
            (bind-window-keys cider-test-report-mode-map)
            (bind-keys :map cider-test-report-mode-map
              ("<tab>"     . forward-button)
              ("<backtab>" . backward-button)
              ("TAB"       . forward-button))))

(defun modify-syntax-entries ()
  "Do not treat valid identifier symbols as word separators."
  (modify-syntax-entry ?- "w")
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?< "w")
  (modify-syntax-entry ?> "w")
  (modify-syntax-entry ?? "w")
  (modify-syntax-entry ?! "w")
  (modify-syntax-entry ?* "w")
  (modify-syntax-entry ?= "w"))

(add-hook 'cider-mode-hook
          (lambda ()
            (eldoc-mode)
            (setq eldoc-echo-area-use-multiline-p nil)
            (cider-company-enable-fuzzy-completion)
            (advice-add 'cider-quit :before #'close-repl-window)))

(add-hook 'cider-repl-mode-hook
          (lambda ()
            (cider-company-enable-fuzzy-completion)
            (modify-syntax-entries)))

(add-hook 'cider--debug-mode-hook
          (lambda ()
            (evil-make-overriding-map cider--debug-mode-map 'normal)
            (evil-normalize-keymaps)))

(defun my-clojure-mode-before-save-hook ()
  "Sort namespaces automatically before saving a Clojure file."
  (when (eq major-mode 'clojure-mode)
    (clojure-sort-ns)))

(add-hook 'before-save-hook #'my-clojure-mode-before-save-hook)

(defun fuco1/lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

- `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

- an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

- a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (init-lisp-common)
            (redshank-mode)
            (setq-local lisp-indent-function #'fuco1/lisp-indent-function)))

(use-package fennel-mode
  :defer)

(use-package glsl-mode
  :defer)

(use-package go-mode
  :defer
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'gofmt-before-save)
              (evil-leader/set-key "h d" 'godoc-at-point)
              (local-set-key (kbd "M-.") 'godef-jump)
              (local-set-key (kbd "M-,") 'pop-tag-mark))))

(use-package flycheck-haskell
  ;; Disabling this package, since it only gives error:
  ;; "Reading Haskell configuration failed with exit code Segmentation fault and
  ;; output:", when trying to run it in Nix/direnv setup.
  :disabled
  :hook (haskell-mode . flycheck-haskell-setup))

(add-hook 'haskell-mode-hook
          (lambda ()
            (rainbow-mode -1)
            (evil-leader/set-key "x h" 'haskell-hoogle)
            (setq evil-shift-width 2)
            (define-key haskell-mode-map (kbd "C-c C-c C-s")
              'haskell-mode-stylish-buffer)
            (setq haskell-auto-insert-module-format-string
                  "module %s\n  () where\n\n")
            (haskell-auto-insert-module-template)
            (smartparens-mode)
            (sp-local-pair 'haskell-mode "{" "}")))

(use-package lsp-haskell
  :hook ((haskell-mode . lsp-deferred)
         (haskell-literate-mode . lsp-deferred)))

(use-package ormolu
  :after haskell-mode
  :config
  (define-key haskell-mode-map (kbd "C-c C-c C-f") 'ormolu-format-buffer))

(use-package j-mode
  :defer
  :init
  (add-to-list 'auto-mode-alist '("\\.ij[rstp]$" . j-mode))
  :custom
  (j-console-cmd "jconsole")
  :config
  (custom-set-faces
   '(j-verb-face ((t (:foreground "Red"))))
   '(j-adverb-face ((t (:foreground "Green"))))
   '(j-conjunction-face ((t (:foreground "Blue"))))
   '(j-other-face ((t (:foreground "Black"))))))

(use-package eclim
  :disabled
  :defer
  :config
  ;; (setq eclimd-autostart t)
  (setq eclimd-autostart-with-default-workspace t)
  (setq eclim-eclipse-dirs "~/eclipse/java-oxygen/eclipse")
  (setq eclim-executable
        (expand-file-name "~/.p2/pool/plugins/org.eclim_2.7.2/bin/eclim"))
  (setq eclimd-default-workspace (expand-file-name "~/eclipse-workspace"))
  (evil-set-initial-state 'eclim-problems-mode 'emacs)
  (evil-set-initial-state 'eclim-project-mode 'emacs)
  (add-hook 'java-mode-hook
            (lambda ()
              ;; `electric-pair-mode' causes extra closing parenthesis to be
              ;; inserted.
              (electric-pair-mode -1)

              (eclim-mode)
              (evil-leader/set-key "e b" 'eclim-project-build)
              (evil-leader/set-key "e c" 'eclim-project-create)
              (evil-leader/set-key "e r" 'eclim-run-class)
              (setq help-at-pt-display-when-idle t)
              (setq help-at-pt-timer-delay 0.1)
              (help-at-pt-set-timer)
              (setq comment-start "//"
                    comment-end "")
              (jco/define-bindings
               java-mode-map
               '(("M-g M-n" . eclim-problems-next-same-file)
                 ("M-g n" . eclim-problems-next-same-file)
                 ("M-g M-p" . eclim-problems-prev-same-file)
                 ("M-g p" . eclim-problems-prev-same-file))))))

(use-package gradle-mode
  :disabled
  :defer
  :config
  (add-hook 'java-mode-hook
            (lambda ()
              (setq gradle-executable-path "/opt/gradle-4.6/bin/gradle")
              (gradle-mode)
              (evil-leader/set-key "g r"
                (lambda ()
                  (interactive)
                  (gradle-run "run")))
              (evil-leader/set-key "t t"
                (lambda ()
                  (interactive)
                  (gradle-run "test --info")))
              (evil-leader/set-key "t s" 'gradle-single-test))))

(setq js-indent-level 2)
(with-eval-after-load 'js
  (define-key js-mode-map (kbd "M-.") nil))

(defun setup-tide ()
  "Set up `tide-mode' for JavaScript and TypeScript."
  (interactive)
  (tide-setup)
  (flycheck-mode)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode)
  (tide-hl-identifier-mode))

(use-package tide
  :hook ((typescript-mode . setup-tide)
         (typescript-mode . tide-hl-identifier-mode))
  :init
  (add-hook 'js-mode-hook #'setup-tide)
  :config
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append))

(use-package jinja2-mode
  :defer)

(use-package json-mode
  :defer)

(use-package slime
  :after lisp-mode
  :config
  (setq slime-description-autofocus t)
  (add-hook 'lisp-mode-hook
            (lambda ()
              (evil-leader/set-key "x s" 'slime)
              (evil-leader/set-key "x r" 'slime-restart-inferior-lisp)))
  (add-hook 'slime-popup-buffer-mode-hook
            (lambda ()
              (evil-motion-state)))
  (add-hook 'slime-repl-mode-hook
            (lambda ()
              (evil-normal-state)))
  (add-hook 'slime-connected-hook
            (lambda ()
              (with-selected-window (get-buffer-window (slime-output-buffer t))
                (let ((height (if (or (jco/at-office-p)
                                      (display-graphic-p)) 15 10)))
                  (jco/move-window-to-bottom height)))))
  (add-hook 'lisp-mode-hook
            (lambda ()
              (when (file-exists-p "~/quicklisp/slime-helper.el")
                (load (expand-file-name "~/quicklisp/slime-helper.el")))
              (init-lisp-common)
              (evil-leader/set-key "h h" 'hyperspec-lookup)
              (redshank-mode)
              (setq-local evil-move-beyond-eol t)
              (modify-syntax-entry ?: "w")
              (modify-syntax-entry ?< "w")
              (modify-syntax-entry ?> "w")
              (modify-syntax-entry ?= "w")
              (modify-syntax-entry ?* "w")
              (setq inferior-lisp-program "sbcl")
              (slime-setup '(slime-asdf slime-company slime-fancy))
              (slime-asdf-init) ;; Required for `slime-load-system'.
              (slime-company-maybe-enable)
              (bind-key (kbd "M-.") 'slime-edit-definition lisp-mode-map)
              (define-key sldb-mode-map "\C-w" 'evil-window-map))))

(use-package slime-company
  :defer)

(defun jco/lisp-comment-dwim ()
  "Comments Lisp sexps smartly."
  (interactive)
  (if (and (not (hlt-nonempty-region-p))
           (member (char-after) '(?\( ?{ ?\[)))
      (progn (mark-sexp)
             (comment-dwim nil))
    (call-interactively #'evilnc-comment-or-uncomment-lines)))

(defun init-lisp-common ()
  "Common configuration options for all Lisp modes."
  (unless (derived-mode-p 'clojure-mode)
    (aggressive-indent-mode))
  (setq evil-shift-width 2)
  (define-key lisp-mode-shared-map (kbd "M-;") #'jco/lisp-comment-dwim)
  ;; do not treat "-" as a word separator
  (modify-syntax-entry ?- "w")
  (smartparens-strict-mode))

(use-package redshank
  :defer
  :init
  (setq redshank-prefix-key "C-c RET"))

(use-package lua-mode
  :defer)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :config
  (setq markdown-command
        (concat "pandoc -c file://"
                (expand-file-name
                 (concat user-emacs-directory "github-pandoc.css"))
                " --from gfm -t html5 --mathjax "
                "--highlight-style=tango --standalone "
                "--metadata title=\"*markdown-output*\"")))

(use-package ox-gfm
  :after markdown-mode)

(use-package mustache-mode
  :mode "\\.mustache\\'")

(use-package nix-mode
  :custom
  (evil-shift-width 2)
  :mode "\\.nix\\'"
  :config
  (setenv "DIRENV_ALLOW_NIX" "1")
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                    :major-modes '(nix-mode)
                    :server-id 'nix))
  ;; Requires `rnix-lsp' binary.
  (add-hook 'nix-mode-hook
            (lambda ()
              ;; do not treat "-" as a word separator
              (modify-syntax-entry ?- "w")

              (lsp-deferred))))

(use-package purescript-mode
  :defer)

(use-package rustic
  :defer
  :config
  (add-hook 'rustic-mode-hook
            (lambda ()
              (sp-pair "\'" nil :actions :rem)
              (modify-syntax-entry ?! "w"))))

(use-package terraform-mode
  :defer
  :config
  (add-hook 'terraform-mode-hook
            (lambda ()
              (setq evil-shift-width terraform-indent-level)
              ;; do not treat "-" as a word separator
              (modify-syntax-entry ?- "w"))))

(use-package toml-mode
  :defer
  :config
  (modify-syntax-entry ?- "w") ;; Do not treat "-" as a word separator.
  )

(use-package typescript-mode
  :mode "\\.tsx\\'")

(defvar jco/rotate-text-rotations
  '(("true" "false")
    ("True" "False")
    ("TRUE" "FALSE")
    ("yes" "no")
    ("Yes" "No")
    ("YES" "NO")
    ("before" "after")
    ("Before" "After")
    ("BEFORE" "AFTER")
    ("begin" "end")
    ("Begin" "End")
    ("BEGIN" "END")
    ("width" "height")
    ("Width" "Height")
    ("WIDTH" "HEIGHT")
    ("x" "y")
    ("X" "Y")
    ("in" "out")
    ("In" "Out")
    ("IN" "OUT")
    ("client" "server")
    ("Client" "Server")
    ("CLIENT" "SERVER")
    ("left" "right")
    ("Left" "Right")
    ("LEFT" "RIGHT")
    ("high" "low")
    ("HIGH" "LOW"))
  "List of text rotation sets.")

(defun rotate-word-at-point ()
  "Rotate word at point based on contents of `jco/rotate-text-rotations'."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word))
        (opoint (point)))
    (when (consp bounds)
      (let ((beg (car bounds))
            (end (copy-marker (cdr bounds))))
        (rotate-region beg end)
        (goto-char (if (> opoint end) end opoint))))))

(defun rotate-region (beg end)
  "Rotate all matches in `jco/rotate-text-rotations' between point and mark."
  (interactive "r")
  (let ((regexp (jco/rotate-convert-rotations-to-regexp
                 jco/rotate-text-rotations))
        (end-mark (copy-marker end)))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward regexp (marker-position end-mark) t)
        (let* ((found (match-string 0))
               (replace (rotate-next found)))
          (replace-match replace))))))

(defun rotate-string (string &optional rotations)
  "Rotate all matches in STRING using associations in ROTATIONS.
If ROTATIONS are not given it defaults to `jco/rotate-text-rotations'."
  (let ((regexp (jco/rotate-convert-rotations-to-regexp
                 (or rotations jco/rotate-text-rotations)))
        (start 0))
    (while (string-match regexp string start)
      (let* ((found (match-string 0 string))
             (replace (rotate-next
                       found
                       (or rotations jco/rotate-text-rotations))))
        (setq start (+ (match-end 0)
                       (- (length replace) (length found))))
        (setq string (replace-match replace nil t string))))
    string))

(defun rotate-next (string &optional rotations)
  "Return the next element after STRING in ROTATIONS."
  (let ((rots (rotate-get-rotations-for
               string
               (or rotations jco/rotate-text-rotations))))
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
                    (or rotations jco/rotate-text-rotations))))

(defun jco/rotate-convert-rotations-to-regexp (rotations)
  (regexp-opt (jco/rotate-flatten-list rotations)))

(defun jco/rotate-flatten-list (list-of-lists)
  "Flatten LIST-OF-LISTS to a single list.
Example:
  (jco/rotate-flatten-list '((a b c) (1 ((2 3)))))
    => (a b c 1 2 3)"
  (if (null list-of-lists)
      list-of-lists
    (if (listp list-of-lists)
        (append (jco/rotate-flatten-list (car list-of-lists))
                (jco/rotate-flatten-list (cdr list-of-lists)))
      (list list-of-lists))))

(defun rk/open-compilation-buffer (&optional buffer-or-name shackle-alist shackle-plist)
  "Helper for selecting window for opening *compilation* buffers."
  ;; Find existing compilation window left of the current window or left-most
  ;; window.
  (let ((win (or (loop for win = (if win (window-left win) (get-buffer-window))
                       when (or (not (window-left win))
                                (string-prefix-p "*compilation" (buffer-name (window-buffer win))))
                       return win)
                 (get-buffer-window))))
    ;; If the window is dedicated to a non-compilation buffer, use the current
    ;; one instead.
    (when (window-dedicated-p win)
      (let ((buf-name (buffer-name (window-buffer win))))
        (unless (string-prefix-p "*compilation" buf-name)
          (setq win (get-buffer-window)))))
    (set-window-buffer win (get-buffer buffer-or-name))
    (set-frame-selected-window (window-frame win) win)))


(use-package shackle
  :defer
  :diminish
  :custom
  (shackle-rules '((compilation-mode :custom rk/open-compilation-buffer :select t)
                   ("\\*Apropos\\|Help\\|Occur\\|tide-references\\*" :regexp t :same t :select t :inhibit-window-quit t)
                   ("\\*magit" :regexp t :same t :select t)
                   ("\\*shell.*" :regexp t :same t :select t)
                   ("\\*PowerShell.*" :regexp t :same t :select t)
                   ("\\*Cargo.*" :regexp t :other t :select nil)
                   ("*Messages*" :select nil :other t)
                   ("*go-guru-output*" :select t :same t)
                   ("*Proced*" :select t :same t)
                   ("*Buffer List*" :select t :same t)
                   ("\\*Pp Eval" :regexp t :same nil :select t :other t)
                   ("*Messages*" :same nil :other t :select t :inhibit-window-quit t)

                   ;; Slime
                   ("*slime-source*" :select nil :same nil :other t)
                   ("*slime-description*" :select nil :other t :inhibit-window-quit t)
                   ("\\*slime-repl" :regexp t :same nil :select nil :other t)
                   ;; ("\\*sldb" :regexp t :other t :inhibit-window-quit t :select t)
                   ("\\*slime-compilation" :regexp t :same nil :select nil :other t)
                   ("*slime-scratch*" :same nil :select t :other t)

                   ;; ert
                   ("*ert*" :select nil :same nil :other t)

                   ;; Clojure
                   ("*sesman CIDER browser*" :inhibit-window-quit t :select t :same t)
                   ("\\*cider-repl" :regexp t :same nil :other t)))
  (shackle-default-rule nil)
  :init
  (shackle-mode))

(defun jco/check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(require 'minibuffer)

(defun jco/tab-indent-or-complete (&optional arg)
  "Expand completion or yas snippet. Prefix ARG is used if in `org-mode'."
  (interactive "P")
  (cond
   ((minibufferp)
    (minibuffer-complete))
   ((derived-mode-p 'magit-mode (symbol-name major-mode))
    (magit-section-toggle (magit-current-section)))
   (t
    (when (or (not yas/minor-mode)
              (null (yas-expand)))
      (if (jco/check-expansion)
          (progn
            (company-manual-begin)
            (when (null company-candidates)
              (company-abort)))
        (when (derived-mode-p 'org-mode)
          (org-cycle arg)))))))

(defun jco/tab-complete-or-next-field ()
  (interactive)
  (if (or (not yas/minor-mode)
          (null (yas-expand)))
      (if company-candidates
          (company-complete-selection)
        (if (jco/check-expansion)
            (progn
              (company-manual-begin)
              (if (null company-candidates)
                  (progn
                    (company-abort)
                    (yas-next-field))))
          (yas-next-field)))))

(defun jco/expand-snippet-or-next-selection ()
  (interactive)
  (if (or (not yas/minor-mode)
          (null (yas-expand))
          (company-abort))

      (if (> company-candidates-length 1)
          (company-select-next)
        (company-complete-common))))

(defun jco/abort-company-or-yas ()
  (interactive)
  (if (null company-candidates)
      (yas-abort-snippet)
    (company-abort)))

(global-set-key [tab] 'jco/tab-indent-or-complete)
(global-set-key (kbd "TAB") 'jco/tab-indent-or-complete)

(with-eval-after-load 'company
  (define-key company-active-map [tab] 'jco/expand-snippet-or-next-selection)
  (define-key company-active-map (kbd "TAB")
    'jco/expand-snippet-or-next-selection))

(with-eval-after-load 'yasnippet
  (define-key yas-minor-mode-map [tab] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  (define-key yas-keymap [tab] 'jco/tab-complete-or-next-field)
  (define-key yas-keymap (kbd "TAB") 'jco/tab-complete-or-next-field)
  (define-key yas-keymap [(control tab)] 'yas-next-field)
  (define-key yas-keymap (kbd "C-g") 'jco/abort-company-or-yas))

(defun org-babel-tangle-config ()
  "Tangle emacs config file.  Uses the following custom logic:

1. Detangle init.el back to org file in order to pick up changes
to custom variables. Should only pick up changes to that block as
that's the only one exported with links enabled.

2. Tangle file with only id type links available. This is a
workaround to prevent git links from being used when in a git
repo."
  (interactive)
  (let ((org-link-parameters '(("id" :follow org-id-open))))
    ;; Read back changes to custom variables in init.el.
    (save-window-excursion
      (org-babel-detangle "init.el"))
    (let
        ;; Avoid infinite recursion.
        ((after-save-hook (remove 'org-babel-tangle-config+ after-save-hook)))
      (org-babel-tangle-file (concat user-emacs-directory "init.org")))))

(use-package iedit
  :defer
  :init
  (evil-leader/set-key ";" #'iedit-mode))

(use-package string-inflection
  :defer
  :init
  (evil-leader/set-key "s i" 'string-inflection-all-cycle)
  (evil-leader/set-key "s s" 'string-inflection-underscore)
  (evil-leader/set-key "s k" 'string-inflection-kebab-case)
  (evil-leader/set-key "s c" 'string-inflection-lower-camelcase)
  (evil-leader/set-key "s C" 'string-inflection-camelcase))

(use-package undo-tree
  :defer
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t))

(require 'kurecolor)

(defmacro install-themes ()
  "Install commonly used theme packages using `use-package'."
  (let ((theme-pkgs
         '(afternoon-theme
           ample-theme
           ample-zen-theme
           borland-blue-theme
           challenger-deep-theme
           cherry-blossom-theme
           chyla-theme
           color-theme-sanityinc-tomorrow
           cyberpunk-theme
           ;; darkane-theme
           darktooth-theme
           doom-themes
           dracula-theme
           eink-theme
           espresso-theme
           flatland-theme
           flatui-theme
           github-modern-theme
           gotham-theme
           grandshell-theme
           gruber-darker-theme
           hemisu-theme
           kaolin-themes
           leuven-theme
           material-theme
           minimal-theme
           molokai-theme
           monokai-theme
           mustang-theme
           nubox
           organic-green-theme
           prassee-theme
           reykjavik-theme
           solarized-theme
           soothe-theme
           tao-theme)))
    `(progn ,@(mapcar (lambda (p)
                        `(use-package ,p :defer))
                      theme-pkgs))))

(progn
  (install-themes)
  (load-theme jco/theme t)

  (set-face-background 'evil-search-highlight-persist-highlight-face
                       "RoyalBlue4")
  (set-face-foreground 'show-paren-match "#101f24")
  (set-face-background 'show-paren-match "#89C5B7")
  ;; (set-face-attribute 'font-lock-comment-face nil :slant 'normal)
  (custom-set-faces
   '(italic ((t (:underline t)))))
  (with-eval-after-load 'smartparens
    (set-face-attribute 'sp-show-pair-match-face nil :weight 'normal)))

(cl-defun jco/current-fg (&optional (adj 0.0))
  "Get the current foreground color, optionally adjusting brightness by ADJ."
  (kurecolor-adjust-brightness (face-attribute 'default :foreground) adj))

(cl-defun jco/current-bg (&optional (adj 0.0))
  "Get the current background color, optionally adjusting brightness by ADJ."
  (kurecolor-adjust-brightness (face-attribute 'default :background) adj))

(cl-case jco/theme
  (adwaita
   (setq sml/theme 'light)
   (set-face-background 'hl-line "#dadfe1")
   (set-face-background 'evil-search-highlight-persist-highlight-face "#e0dcbe")
   (with-eval-after-load 'smartparens
     (set-face-background 'sp-pair-overlay-face "LightBlue"))
   (setq jco/cursor-color  "#101f24")
   (with-eval-after-load 'mu4e
     (set-face-background 'mu4e-highlight-face "#7ceece")
     (set-face-foreground 'mu4e-header-highlight-face "#101f24")))

  (borland-blue
   (set-face-background 'region "RoyalBlue4")
   (with-eval-after-load 'company
     (set-face-background 'company-tooltip-selection "#268bd2")))

  (challenger-deep
   (set-face-background 'hl-line "#352e5a")
   (set-face-background 'line-number-current-line nil)
   (set-face-foreground 'line-number-current-line nil)
   (with-eval-after-load 'volatile-highlights
     (set-face-background 'vhl/default-face "#2f333c"))
   (with-eval-after-load 'smartparens
     (set-face-background 'sp-show-pair-match-face "#268bd2"))
   (with-eval-after-load 'calfw
     (set-face-background 'cfw:face-toolbar-button-on "Steelblue4")
     (set-face-background 'cfw:face-toolbar-button-off "Steelblue4")
     (set-face-foreground 'cfw:face-toolbar-button-off "#dadfe1")))

  (chyla
   (setq sml/theme 'light)
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "LightBlue"))

  (darkane
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "midnightblue")
   (set-face-background 'hl-line "#041040"))

  (doom-1337
   (set-face-background 'region "#582c6b")
   (with-eval-after-load 'tide
     (set-face-background 'tide-hl-identifier-face (jco/current-bg 0.25))) 
   (with-eval-after-load 'mu4e
     (set-face-foreground 'mu4e-highlight-face (jco/current-bg -0.10)))
   (with-eval-after-load 'vertico
     (set-face-background 'vertico-current "#406080")))

  (doom-acario-light
   (set-face-attribute 'font-lock-comment-face nil :slant 'normal)
   (set-face-attribute 'font-lock-comment-face nil :inherit nil)
   (with-eval-after-load 'volatile-highlights
     (set-face-background 'vhl/default-face (jco/current-bg -0.10)))
   (with-eval-after-load 'magit
     (set-face-background 'magit-diff-hunk-heading "#f0f0f0")
     (set-face-background 'magit-diff-hunk-heading-highlight "#e0e0e0")
     (set-face-background 'magit-diff-added-highlight "#e0e0e0")
     (set-face-background 'magit-diff-removed-highlight "#e0e0e0"))
   (with-eval-after-load 'mu4e
     (set-face-foreground 'mu4e-highlight-face (jco/current-bg -0.10))))

  (doom-challenger-deep
   (set-face-background 'hl-line "#352e5a")
   (set-face-background 'line-number-current-line nil)
   (set-face-foreground 'line-number-current-line nil)
   (with-eval-after-load 'volatile-highlights
     (set-face-background 'vhl/default-face "#2f333c"))
   (with-eval-after-load 'smartparens
     (set-face-background 'sp-show-pair-match-face "#268bd2"))
   (with-eval-after-load 'calfw
     (set-face-background 'cfw:face-toolbar-button-on "Steelblue4")
     (set-face-background 'cfw:face-toolbar-button-off "Steelblue4")
     (set-face-foreground 'cfw:face-toolbar-button-off "#dadfe1")))

  (doom-dracula
   (set-face-background 'hl-line "#3f525b")
   (set-face-background 'region "#582c6b"))

  (doom-flatwhite
   (with-eval-after-load 'volatile-highlights
     (set-face-background 'vhl/default-face "#e0dcbe")))

  (doom-gruvbox
   (set-face-background 'hl-line "#3e3c3a")
   (with-eval-after-load 'volatile-highlights
     (set-face-background 'vhl/default-face (jco/current-bg 0.10)))
   (set-face-background 'highlight "#ccae62")
   (with-eval-after-load 'company-box
     (set-face-background 'company-box-scrollbar "#7ceece"))
   (with-eval-after-load 'mu4e
     (set-face-foreground 'mu4e-highlight-face "black")))

  (doom-henna
   (set-face-background 'evil-search-highlight-persist-highlight-face "#6ab04c")
   (with-eval-after-load 'vertico
     (set-face-foreground 'vertico-group-separator "#808080")
     (set-face-foreground 'vertico-group-title "#c0c0c0")))

  (doom-laserwave
   (set-face-background 'hl-line (jco/current-bg 0.10))
   (with-eval-after-load 'eyebrowse
     (set-face-foreground 'eyebrowse-mode-line-active "#808080")))

  (doom-molokai
   (with-eval-after-load 'mu4e
     (set-face-foreground 'mu4e-highlight-face "#101f24"))
   (with-eval-after-load 'smartparens
     (set-face-background 'sp-show-pair-match-face "#6ab04c")))

  (doom-moonlight
   (set-face-background 'highlight "#22a7f0")
   (set-face-background 'region (jco/current-bg 0.30))
   (with-eval-after-load 'vertico
     (set-face-foreground 'vertico-group-separator "#808080")
     (set-face-foreground 'vertico-group-title "#c0c0c0"))
   (with-eval-after-load 'mu4e
     (set-face-foreground 'mu4e-highlight-face
                          (face-attribute 'default :background))))

  (doom-oceanic-next
   (with-eval-after-load 'volatile-highlights
     (set-face-background 'vhl/default-face (jco/current-bg 0.10))))

  (doom-old-hope
   (with-eval-after-load 'mu4e
     (set-face-foreground 'mu4e-highlight-face "#101f24")))

  (doom-one-light
   (set-face-background 'evil-search-highlight-persist-highlight-face "#e6ffe6")
   (set-face-background 'highlight "#e6ffe6")
   (with-eval-after-load 'volatile-highlights
     (set-face-background 'vhl/default-face (jco/current-bg -0.10))))

  (doom-opera-light
   (set-face-background 'highlight "#e4f1fe"))

  ((doom-solarized-light solarized-light)
   (setq sml/theme 'light)
   (set-face-background 'region "#e0dcbe")
   (with-eval-after-load 'cider
     (set-face-background 'cider-deprecated-face "#e0dcbe"))
   (set-face-background 'evil-search-highlight-persist-highlight-face "#f9bf3b")
   (set-face-background 'lazy-highlight "#f9bf3b")
   (with-eval-after-load 'mu4e
     (set-face-foreground 'mu4e-highlight-face "#101f24")
     (set-face-background 'mu4e-highlight-face "#7ceece")))

  (doom-tomorrow-day
   (set-face-background 'highlight "#e4f1fe")
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "#e4f1fe"))

  (dracula
   (set-face-background 'region "#582c6b"))

  (eink
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "LightBlue"))

  (github-modern
   (setq sml/theme 'light)
   (set-face-foreground 'avy-lead-face "#00b894")
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        (face-attribute 'highlight :background))
   (set-face-background 'header-line "#e4f1fe")
   (set-face-foreground 'header-line "Black")
   (set-face-background 'hl-line "#f5f6fa")
   (set-face-foreground 'hydra-face-amaranth "#e0dcbe")
   (set-face-foreground 'hydra-face-red "Red")
   (set-face-foreground 'hydra-face-blue "Blue")
   (set-face-foreground 'hydra-face-pink "#fd79a8")
   (set-face-foreground 'isearch-fail "#d63031")
   (set-face-background 'region "#e4f1fe")
   (set-face-background 'whitespace-empty "#ffe9ec")
   (set-face-background 'whitespace-trailing "#ffe9ec")
   (set-face-background 'whitespace-tab "#f0f0f0")
   (with-eval-after-load 'cider
     (set-face-background 'cider-deprecated-face "#d63031")
     (set-face-background 'cider-test-error-face "Red")
     (set-face-foreground 'cider-test-error-face "Black")
     (set-face-background 'cider-test-failure-face "Red")
     (set-face-foreground 'cider-test-failure-face "Black")
     (set-face-background 'cider-test-success-face "Green"))
   (with-eval-after-load 'company
     (set-face-background 'company-tooltip-selection "#fd79a8")
     (set-face-background 'company-tooltip-annotation-selection "#fd79a8")
     (set-face-foreground 'company-preview-common "#f0f0f0"))
   (with-eval-after-load 'ediff
     (set-face-foreground 'ediff-current-diff-Ancestor "white")
     (set-face-foreground 'ediff-current-diff-C "white")
     (set-face-foreground 'ediff-fine-diff-Ancestor "white")
     (set-face-foreground 'ediff-fine-diff-C "white"))
   (with-eval-after-load 'magit
     (set-face-foreground 'magit-blame-name "White")
     (set-face-foreground 'magit-blame-date "White")
     (set-face-foreground 'magit-blame-hash "White")
     (set-face-foreground 'magit-blame-summary "White")
     (set-face-foreground 'magit-blame-heading "White")
     (set-face-background 'magit-diff-hunk-heading "#f0f0f0")
     (set-face-background 'magit-diff-hunk-heading-highlight "#c0c0c0")
     (set-face-foreground 'magit-popup-argument "#22a7f0"))
   (with-eval-after-load 'mu4e
     (set-face-background 'mu4e-highlight-face "#e4f1fe"))
   (with-eval-after-load 'smartparens
     (set-face-background 'sp-show-pair-match-face "#fda7df"))
   (with-eval-after-load 'smart-mode-line
     (set-face-foreground 'sml/modified "Blue")))

  (gotham
   (setq jco/cursor-color "LightBlue")
   (set-face-background 'evil-search-highlight-persist-highlight-face "#e0dcbe")
   (set-face-foreground 'evil-search-highlight-persist-highlight-face
                        "#101f24"))

  (hemisu-light
   (setq sml/theme 'light)
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "LightBlue"))

  (kaolin-eclipse
   (set-face-background 'hl-line "#3E2A3E")
   (set-face-background 'region "#582c6b")
   (with-eval-after-load 'volatile-highlights
     (set-face-background 'vhl/default-face "#3E2A3E"))
   (with-eval-after-load 'mu4e
     (set-face-foreground 'mu4e-highlight-face "#101f24")
     (set-face-background 'mu4e-highlight-face "#7ceece")))

  (kaolin-ocean
   (set-face-background 'hl-line "#1A2631")
   (with-eval-after-load 'volatile-highlights
     (set-face-background 'vhl/default-face "#2B2C40"))
   (with-eval-after-load 'mu4e
     (set-face-foreground 'mu4e-highlight-face "#101f24")
     (set-face-background 'mu4e-highlight-face "#7ceece")))

  (material
   (set-face-background 'hl-line "#37474f")
   (set-face-background 'evil-search-highlight-persist-highlight-face "#e0dcbe")
   (with-eval-after-load 'company
     (set-face-background 'company-tooltip (jco/current-bg 0.25))
     (set-face-foreground 'company-tooltip-common (jco/current-fg))
     (set-face-background 'company-tooltip-selection "#fd79a8")
     (set-face-background 'company-tooltip-annotation-selection "#fd79a8")
     (set-face-foreground 'company-preview-common "#f0f0f0"))
   (with-eval-after-load 'org
     (set-face-background 'org-todo nil)))

  (material-light
   (setq sml/theme 'light)
   (setq jco/cursor-color "azure4")
   (set-face-background 'hl-line "Gray75"))

  (meacupla
   (setq jco/cursor-color "azure4")
   (set-face-background 'hl-line "Gray80")
   (set-face-background 'whitespace-trailing "IndianRed1"))

  (minimal
   (set-face-background 'hl-line "#101f24")
   (set-face-foreground 'mode-line-emphasis "#95a5a6")
   (with-eval-after-load 'org
     (set-face-background 'org-todo nil)
     (set-face-background 'evil-search-highlight-persist-highlight-face
                          "DarkOrange4")))

  (minimal-light
   (set-face-background 'region "LightBlue")
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "LightBlue")
   (set-face-foreground 'mode-line-emphasis "#74b9ff"))

  (molokai
   (set-face-foreground 'font-lock-comment-face "azure4")
   (with-eval-after-load 'smartparens
     (set-face-background 'sp-pair-overlay-face "#582c6b"))
   (set-face-background 'region "#582c6b")
   (set-face-background 'evil-search-highlight-persist-highlight-face "#f9bf3b")
   (set-face-foreground 'evil-search-highlight-persist-highlight-face "#465457")
   (set-face-background 'lazy-highlight "#f9bf3b")
   (set-face-background 'ffap "#582c6b")
   (with-eval-after-load 'mu4e
     (set-face-background 'mu4e-highlight-face "#582c6b")
     (set-face-foreground 'mu4e-highlight-face "#ececec")
     (set-face-foreground 'mu4e-header-highlight-face "#ececec")))

  (mustang
   (setq jco/cursor-color "#ececec")
   (set-face-background 'region "#582c6b")
   (set-face-background 'lazy-highlight "VioletRed3")
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "#e0dcbe")
   (set-face-foreground 'evil-search-highlight-persist-highlight-face
                        "#101f24")
   (set-face-foreground 'font-lock-warning-face "#ff6523")
   (set-face-background 'font-lock-warning-face nil)
   (with-eval-after-load 'volatile-highlights
     (set-face-background 'vhl/default-face "#582c6b")))

  (nubox-dark
   (set-face-background 'hl-line "#2a2d2e")
   (with-eval-after-load 'volatile-highlights
     (set-face-background 'vhl/default-face "#2a2d2e"))
   (set-face-background 'iedit-occurrence "#2a2d2e")
   (with-eval-after-load 'smartparens
     (set-face-background 'sp-pair-overlay-face "#444748"))
   (set-face-background 'region "#582c6b")
   (set-face-background 'ffap "#582c6b")
   (set-face-background 'highlight "#582c6b"))

  (nubox-light
   (setq sml/theme 'light)
   (setq jco/cursor-color "#101f24")
   (set-face-background 'hl-line "#e0dcbe")
   (with-eval-after-load 'volatile-highlights
     (set-face-background 'vhl/default-face "#e0dcbe"))
   (with-eval-after-load 'smartparens
     (set-face-background 'sp-pair-overlay-face "#c7c3a5"))
   (set-face-background 'region "#ffc3ff")
   (set-face-background 'ffap "#ffc3ff")
   (set-face-background 'evil-search-highlight-persist-highlight-face "#f9bf3b")
   (with-eval-after-load 'ledger-mode
     (set-face-background 'ledger-font-xact-highlight-face "#e0dcbe")
     (set-face-background 'ledger-occur-xact-face "#e0dcbe"))

   (with-eval-after-load 'mu4e
     (set-face-background 'mu4e-highlight-face "#7ceece")
     (set-face-foreground 'mu4e-header-highlight-face "#101f24")))

  (organic-green
   (setq sml/theme 'light)
   (setq jco/cursor-color "gray25")
   (set-face-background 'evil-search-highlight-persist-highlight-face "#7ceece")
   (set-face-background 'show-paren-match "#c0c060"))

  (prassee
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "VioletRed4"))

  (reykjavik
   (setq jco/cursor-color  "#7ceece")
   (set-face-background 'region "#1a4550")
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "#821800"))

  ((sanityinc-tomorrow-bright sanityinc-tomorrow-day
                              sanityinc-tomorrow-eighties
                              sanityinc-tomorrow-night)
   (setq jco/cursor-color "snow")
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "RoyalBlue4"))

  (sanityinc-tomorrow-blue
   (setq jco/cursor-color "snow")
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "RoyalBlue")
   (with-eval-after-load 'company
     (set-face-background 'company-tooltip-selection "snow")
     (set-face-foreground 'company-tooltip-selection "gray8")
     (set-face-foreground 'company-tooltip-common-selection "VioletRed4")
     (set-face-background 'company-scrollbar-fg "LightBlue")))

  (solarized-dark
   (set-face-background 'region "#1a4550"))

  (tao-yang
   (setq sml/theme 'light)
   (setq jco/cursor-color "azure4")
   (set-face-background 'hl-line "#e1dcd3")
   (set-face-background 'region "#f1dddc")
   (set-face-background 'evil-search-highlight-persist-highlight-face "#b8d8e0")
   (with-eval-after-load 'mu4e
     (set-face-background 'mu4e-header-highlight-face "#b8d8e0"))
   (custom-set-faces
    '(sp-show-pair-match-face ((t (:box nil))))
    '(font-lock-function-name-face ((t (:weight bold :box nil))))))

  (tao-yin
   (set-face-background 'region "#4a3f51")
   (setq jco/cursor-color "#e0dcbe")
   (set-face-background 'hl-line "gray16")
   (set-face-background 'evil-search-highlight-persist-highlight-face
                        "MidnightBlue")
   (custom-set-faces
    '(sp-show-pair-match-face ((t (:box nil))))
    '(font-lock-function-name-face ((t (:weight bold :box nil)))))))

(when (not (display-graphic-p))
  ;; Transparent background in console mode.
  (set-face-background 'default "unspecified-bg")
  (global-hl-line-mode -1)
  (when (fboundp 'nlinum-mode)
    (global-nlinum-mode -1))
  (if (boundp 'magit-mode)
      (set-face-background 'magit-section-highlight nil)
    (with-eval-after-load 'magit
      (set-face-background 'magit-section-highlight nil))))

;; Make syntax highlighting work also for current line.
(set-face-foreground 'highlight nil)

;; ... And selected region.
(set-face-foreground 'region nil)

(set-face-foreground 'minibuffer-prompt "#263238")
(set-face-background 'minibuffer-prompt "#afd700")

;; Fix annoyingly dark backgrounds of dired-subtree faces.
(with-eval-after-load 'dired+
  (let* ((ns (number-sequence 1 5))
         (f  (lambda (x)
               (intern (format "dired-subtree-depth-%d-face" x))))
         (ss (cl-map 'cons f ns)))
    (dolist (f ss)
      (set-face-background f nil))))

(when (not (eq jco/theme 'cyberpunk))
  (let ((info-bg "gray16"))
    (with-eval-after-load 'info+
      (dolist (f '(info-command-ref-item
                   info-constant-ref-item
                   info-file
                   info-function-ref-item
                   info-macro-ref-item
                   info-reference-item
                   info-special-form-ref-item
                   info-syntax-class-item
                   info-user-option-ref-item
                   info-variable-ref-item))
        (set-face-background f info-bg)))))

(when (boundp 'jco/cursor-color)
  (require 'evil-states)
  (setq evil-normal-state-cursor `(,jco/cursor-color box))
  (setq evil-insert-state-cursor `(,jco/cursor-color bar)))

(blink-cursor-mode -1)

(unless (display-graphic-p)
  (custom-set-faces
   '(company-preview
     ((t (:foreground "darkgray" :underline t))))
   '(company-preview-common
     ((t (:inherit company-preview))))
   '(company-tooltip
     ((t (:background "lightgray" :foreground "black"))))
   '(company-tooltip-selection
     ((t (:background "steelblue" :foreground "white"))))
   '(company-tooltip-common
     ((((type x)) (:inherit company-tooltip :weight bold))
      (t (:inherit company-tooltip))))
   '(company-tooltip-common-selection
     ((((type x)) (:inherit company-tooltip-selection :weight bold))
      (t (:inherit company-tooltip-selection))))))

(set-face-background 'whitespace-tab nil)
(set-face-background 'whitespace-indentation nil)

(menu-bar-mode -1)
;; Change for different username.
(setq inhibit-startup-echo-area-message "jco")
(setq inhibit-startup-message t)

(use-package darkroom
  :defer)

(use-package helpful
  :defer
  :init
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  :config
  (add-hook 'helpful-mode-hook
            (lambda ()
              ;; do not treat "-" as a word separator
              (modify-syntax-entry ?- "w"))))

(use-package olivetti
  :defer
  :config
  (setq-default olivetti-body-width (+ fill-column 1))
  (add-hook 'olivetti-mode-hook
            (lambda ()
              (setq display-line-numbers nil))))

(use-package visual-fill-column
  :defer)

(use-package which-key
  :defer
  :config
  (which-key-mode))

(defun jco/yank-current-filename ()
  "Yank the filename of the current buffer to the kill ring."
  (interactive)
  (kill-new (buffer-file-name)))

(defun jco/insert-current-filename (arg)
  "Insert filename of the current buffer at point.
If ARG is given, only the basename (no path and no extension) of the file is
inserted."
  (interactive "P")
  (insert (if arg
              (file-name-base (buffer-file-name))
            (buffer-file-name))))

(defun jco/insert-date (arg)
  "Insert date at current point, in format 2016-11-23.
If ARG is given, dots are used instead of dashes."
  (interactive "P")
  (insert (if arg
              (format-time-string "%d.%m.%Y")
            (format-time-string "%Y-%m-%d"))))

(defun jco/insert-timestamp (arg)
  "Insert timestamp at current point.
The format of the timestamp is \"2021-03-03 Wed 14:31\". If ARG
is given, the format of the timestamp is 2016-11-23T00:00:00 (in
accordance with ISO 8601)."
  (interactive "P")
  (insert (if arg
              (format-time-string "%Y-%m-%dT%H:%M:%S")
            (format-time-string "%Y-%m-%d %a %H:%M"))))

(defun jco/json-lint ()
  "Pretty format JSON."
  (interactive)
  (save-restriction
    (widen)
    (shell-command-on-region (point-min) (point-max) "python -m json.tool"
                             t t)))


(global-set-key (kbd "<f12>") (lambda ()
                                (interactive)
                                (message "Current major mode: %s" major-mode)))

;; See: https://youtu.be/UtqE-lR2HCA?t=5039
(defun jco/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name, delete up to parent
  directory. Otherwise, delete a character backward."
  (interactive "p")
  (if minibuffer-completing-file-name
      (if (string-match-p "/." (minibuffer-contents))
          (if (string-match-p ".*/$" (minibuffer-contents))
              (zap-up-to-char (- arg) ?/)
            (delete-backward-char arg))
        (delete-minibuffer-contents))
    (delete-backward-char arg)))

;; Source: https://karthinks.com/software/fifteen-ways-to-use-embark
(eval-when-compile
  (defmacro my/embark-split-action (fn split-type)
    `(defun ,(intern (concat "my/embark-"
                             (symbol-name fn)
                             "-"
                             (car (last  (split-string
                                          (symbol-name split-type) "-"))))) ()
       (interactive)
       (funcall #',split-type)
       (call-interactively #',fn))))

(use-package vertico
  :defer
  :bind (:map minibuffer-local-map
         ("<backspace>" . jco/minibuffer-backward-kill))
  :custom
  (completion-ignore-case t)
  :init
  (vertico-mode)
  (setq vertico-cycle t))

(eval-and-compile
  (defun my-vertico-extensions-load-path ()
    (concat (file-name-directory (locate-library "vertico")) "extensions/")))

(use-package vertico-directory
  :straight nil
  :load-path (lambda () (list (my-vertico-extensions-load-path)))
  :after vertico
  :bind (:map vertico-map
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :after vertico
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package savehist
  :after vertico
  :init
  (savehist-mode))

(defun my-consult-find-fd (&optional dir initial)
  (interactive "P")
  (let ((consult-find-command "fd --color=never --hidden --full-path ARG OPTS"))
    (consult-find dir initial)))

(defun my-consult-find-git (&optional dir initial)
  (interactive "P")
  (let ((consult-find-command "git ls-files --full-name OPTS -- *ARG*"))
    (consult-find dir initial)))

(defun my-consult-locate-mdfind (&optional initial)
  (interactive "P")
  (let ((consult-locate-command "mdfind -name OPTS ARG"))
    (consult-locate initial)))

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos)
         ("C-c n" . consult-ripgrep)
         ("C-c e" . my-consult-find-git)
         ("C-c i" . my-consult-locate-mdfind)
         ("C-c o" . consult-git-grep)
         ("C-c b" . consult-bookmark)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ("M-s g" . consult-grep)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)   ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch) ;; orig. isearch-edit-string
         ("M-s l" . consult-line)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :custom
  (consult-ripgrep-command
   "rg --null --line-buffered --color=ansi --max-columns=1000 --no-heading --line-number --smart-case . -e ARG OPTS")
  (consult-narrow-key (kbd "<"))
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        register-preview-delay 0
        register-preview-function #'consult-register-format
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (advice-add #'register-preview :override #'consult-register-window)
  (evil-leader/set-key "b" 'consult-buffer)
  (evil-leader/set-key ". r"
    (lambda ()
      (interactive)
      (consult-ripgrep (projectile-project-root))))
  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))

  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  ;; Optionally replace the key help with a completing-read interface.
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers.
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (define-key embark-file-map
    (kbd "2") (my/embark-split-action find-file split-window-below))
  (define-key embark-buffer-map
    (kbd "2") (my/embark-split-action switch-to-buffer split-window-below))
  (define-key embark-bookmark-map
    (kbd "2") (my/embark-split-action bookmark-jump split-window-below))

  (define-key embark-file-map (kbd "3")
    (my/embark-split-action find-file split-window-right))
  (define-key embark-buffer-map (kbd "3")
    (my/embark-split-action switch-to-buffer split-window-right))
  (define-key embark-bookmark-map (kbd "3")
    (my/embark-split-action bookmark-jump split-window-right)))

(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package ethan-wspace
  :defer
  :custom
  (mode-require-final-newline nil)
  :config
  (global-set-key (kbd "M-<backspace>") 'ethan-wspace-clean-all)
  (global-set-key (kbd "M-S-<backspace>") 'delete-trailing-whitespace)

  (add-hook 'makefile-mode-hook 'jco/))

(use-package whitespace
  :defer
  :config
  (setq-default whitespace-style '(face tabs trailing
                                        space-before-tab indentation
                                        empty space-after-tab tab-mark))
  (add-hook 'after-init-hook
            (lambda ()
              (set-face-background 'whitespace-trailing "#82589f"))))

(use-package yasnippet
  :defer
  :config
  (yas-global-mode)
  (setq yas-snippet-dirs (list (concat user-emacs-directory "snippets")))
  ;; yas-indent-line has to be nil to avoid error when expanding `db' snippet.
  (setq yas-indent-line nil)
  (setq yas-also-auto-indent-first-line t)
  (yas-reload-all) ;; Needed to unload snippets in elpa dir.
  (add-hook 'snippet-mode-hook
            (lambda ()
              (modify-syntax-entry ?- "w")
              (ethan-wspace-mode -1)))
  (evil-leader/set-key "TAB" 'yas-insert-snippet))