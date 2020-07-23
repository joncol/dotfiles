;;; #init-spdlog.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(setq (make-local-variable 'jco/spdlog-font-lock-keywords)
      `(("\\[info\\]\\|\\[debug\\]\\|\\[error\\]" . font-lock-constant-face)

        ("\\b[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}" .
         font-lock-builtin-face)

        ("\\b[[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}.[[:digit:]]\\{3\\}" .
         font-lock-constant-face)))

(define-derived-mode spdlog-mode fundamental-mode "spdlog"
  "Major mode for displaying spdlog files."
  (setq font-lock-defaults '((jco/spdlog-font-lock-keywords))))

(provide 'init-spdlog)

;;; init-spdlog.el ends here
