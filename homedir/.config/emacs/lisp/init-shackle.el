;;; #init-shackle.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;; Taken from: https://gist.github.com/rksm/8c07d9ccc9e15adf752d3dd73dd9a61e.

;;; Code:

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
  :ensure
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

(provide 'init-shackle)

;;; init-shackle.el ends here
