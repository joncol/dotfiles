;;; #init-arduino.el --- Arduino configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package arduino-mode
  :straight (arduino-mode :host github
                          :repo "stardiviner/arduino-mode"
                          :upstream (:host github
                                     :repo "bookest/arduino-mode"))
  :defer t)

(use-package company-arduino
  :defer t
  :config
  (setq company-arduino-home "/opt/arduino-1.8.5")
  (add-hook 'irony-mode-hook 'company-arduino-turn-on))

(defun my-company-c-headers-get-system-path ()
  "Return the system include path for the current buffer."
  (let ((default '("/usr/include/" "/usr/local/include/")))
    (company-arduino-append-include-dirs default t)))

(use-package company-c-headers
  :defer t
  :config
  (setq company-c-headers-path-system 'my-company-c-headers-get-system-path)
  (add-to-list 'company-backends 'company-c-headers))

(provide 'init-arduino)

;;; init-arduino.el ends here
