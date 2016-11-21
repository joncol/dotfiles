;;; Set name and email
(use-package s
  :config
  (setq user-mail-address
        (concat (s-replace " " "." (downcase user-full-name)) "@"
                (if (jco/at-office-p)
                    "orzone.com"
                  "mykolab.com"))))

(provide 'init-id)
