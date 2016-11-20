(let* ((acc (jco/irc-account))
       (username (car acc))
       (password (cdr acc)))

  (setq circe-default-user username)
  (setq circe-default-nick username)

  (setq circe-network-options
       `(("Freenode"
          :nickserv-password ,password
          :realname ,username
          :host "irc.freenode.net"
          :port "6697"
          :channels ("#emacs")))))

(setq circe-reduce-lurker-spam t)

(provide 'init-circe)
