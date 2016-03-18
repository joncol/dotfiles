(add-hook 'nsis-mode-hook
          (lambda ()
            (fci-mode)
            (no-final-newline)))

(provide 'init-nsis)
