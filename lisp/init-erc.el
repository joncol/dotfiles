(when (file-exists-p "~/.my_erc_account")
  (let ((acc (jco/read-lines "~/.my_erc_account")))
    (setq erc-nick (car acc))
    (setq erc-password (nth 1 acc))))

(add-hook 'erc-mode-hook
          (lambda ()
            (setq erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
            (setq erc-lurker-threshold-time 3600)
            (setq erc-autojoin-channels-alist
                  '(("freenode.net" "#emacs" "#haskell" "#programming")))))

(provide 'init-erc)
