(defun jco/common-prog ()
  (rainbow-delimiters-mode)
  (modify-syntax-entry ?_ "w") ;; do not treat "_" as a word separator
  (local-set-key (kbd "C-c p s a") 'helm-ag-project-root)
  (fci-mode))

(provide 'init-custom-funs)
