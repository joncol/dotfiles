(let ((acc (jco/read-lines "~/.my_erc_account")))
  (setq erc-nick (car acc))
  (setq erc-password (nth 1 acc)))

(provide 'init-erc)
