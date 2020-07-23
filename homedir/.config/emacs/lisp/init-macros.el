;;; #init-macros.el --- Macro definitions -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(fset 'jco/paste-over [?\" ?0 ?p])

(fset 'jco/paste-over-word [?v ?i ?w ?\" ?0 ?p])

(evil-leader/set-key "p" 'jco/paste-over)
(evil-leader/set-key "P" 'jco/paste-over-word)

(provide 'init-macros)

;;; init-macros.el ends here
