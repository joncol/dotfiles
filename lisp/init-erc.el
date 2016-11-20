(let ((acc (jco/irc-account)))
  (setq erc-nick (car acc))
  (setq erc-password (cdr acc)))

(add-hook 'erc-mode-hook
          (lambda ()
            ;; Automatically identify with NickServ.
            (require 'erc-services)
            (erc-services-mode)
            (setq erc-prompt-for-nickserv-password nil)
            (setq erc-nickserv-passwords
                  `((freenode ((,erc-nick . ,erc-password)))))

            (setq erc-interpret-mirc-color t)

            (setq erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
            (setq erc-lurker-threshold-time 3600)
            (setq erc-autojoin-channels-alist
                  '(("freenode.net" "#emacs" "#haskell" "#programming")))))

(provide 'init-erc)
