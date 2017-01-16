(use-package langtool
  :init
  (setq langtool-language-tool-jar
        "/opt/LanguageTool-3.5/languagetool-commandline.jar"))

(defun langtool-autoshow-detail-popup (overlays)
  (when (require 'popup nil t)
    ;; Do not interrupt current popup.
    (unless (or popup-instances
                ;; Suppress popup after typing `C-g` .
                (memq last-command '(keyboard-quit)))
      (let ((msg (langtool-details-error-message overlays)))
        (popup-tip msg)))))

(setq langtool-autoshow-message-function
      'langtool-autoshow-detail-popup)

(require 'sdcv-mode)
(global-set-key (kbd "C-c d") 'sdcv-search)

(provide 'init-lang)
