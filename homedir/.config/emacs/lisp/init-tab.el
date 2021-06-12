;;; init-tab.el --- Setup tab key functionality

;;; Commentary:
;; Most code taken from
;; http://emacs.stackexchange.com/questions/7908/
;; how-to-make-yasnippet-and-company-work-nicer.

;;; Code:

(defun jco/check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(require 'minibuffer)

(defun jco/tab-indent-or-complete ()
  (interactive)
  (cond
   ((minibufferp)
    (minibuffer-complete))
   ((s-starts-with? "magit-" (symbol-name major-mode))
    (magit-section-toggle (magit-current-section)))
   (t
    (if (or (not yas/minor-mode)
            (null (yas-expand)))
        (if (jco/check-expansion)
            (progn
              (company-manual-begin)
              (if (null company-candidates)
                  (company-abort))))))))

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
      (company-select-next)))

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
  (define-key yas-minor-mode-map (kbd "TAB") nil))

(define-key yas-keymap [tab] 'jco/tab-complete-or-next-field)
(define-key yas-keymap (kbd "TAB") 'jco/tab-complete-or-next-field)
(define-key yas-keymap [(control tab)] 'yas-next-field)
(define-key yas-keymap (kbd "C-g") 'jco/abort-company-or-yas)

;; (with-eval-after-load 'monky
;;   (bind-key [tab] 'monky-toggle-section monky-mode-map))

;; (bind-key [tab] 'completion-at-point read-expression-map)

(provide 'init-tab)

;;; init-tab.el ends here
