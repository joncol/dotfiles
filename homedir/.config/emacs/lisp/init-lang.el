;;; #init-lang.el --- Language config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package flyspell-correct-ivy
  :after flyspell
  :bind (:map flyspell-mode-map
         ("C-;" . flyspell-correct-wrapper)))

(use-package langtool
  :defer t
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
  :straight t
  :ensure nil
  :config
  (global-set-key (kbd "C-c d") 'sdcv-search-input))

(provide 'init-lang)

;;; init-lang.el ends here
