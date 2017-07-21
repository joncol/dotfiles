;;; #init-id.el --- User specific config -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

;;; Set name and email

(use-package s
  :config
  (setq user-mail-address
        (concat (s-replace " " "." (downcase user-full-name)) "@"
                (if (jco/at-office-p)
                    "zimpler.com"
                  "mykolab.com"))))

(provide 'init-id)

;;; init-id.el ends here
