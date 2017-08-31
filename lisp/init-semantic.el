(add-hook 'c-mode-common-hook
          (lambda ()
            (when (not (eq system-type 'gnu/linux))
              (flycheck-mode -1)

              (with-eval-after-load 'c++
                (require 'semantic))

              (require 'semantic/bovine/gcc)
              (add-hook 'semantic-init-hooks 'semantic-reset-system-include)
              (semantic-mode)

              (dolist (mode '(global-semanticdb-minor-mode
                              global-semantic-idle-scheduler-mode
                              global-semantic-idle-summary-mode))
                (add-to-list 'semantic-default-submodes mode))

              (defadvice semantic-symref (around no-confirmation activate)
                (flet  ((yes-or-no-p (&rest args) t)
                        (y-or-n-p (&rest args) t))
                  ad-do-it)))))

(provide 'init-semantic)
