;;; #init-font.el --- Initialize font -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(cond
 ((and (eq system-type 'windows-nt) (display-graphic-p))
  (add-to-list 'default-frame-alist
               '(font . "Hack-10"))
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 100 60))

 ((and (eq system-type 'gnu/linux) (display-graphic-p))
  (add-to-list 'default-frame-alist
               '(font . "FiraCodeMedium-11"))
  ;; (if (>= (x-display-pixel-height) 2160)
  ;;     (set-face-attribute 'default nil :height 140)
  ;;   (set-face-attribute 'default nil :height 110))
  (set-frame-size (selected-frame) 93 64))

 ((eq system-type 'darwin)
  (setq mac-right-option-modifier 'none)
  (when (display-graphic-p)
    (if (<= (x-display-pixel-height) 900)
        (set-frame-size (selected-frame) 93 47)
      (set-frame-size (selected-frame) 93 60))
    (set-face-attribute 'default nil :height 145))))

(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
  :config
  ;; Enable the "www" ligature in every possible major mode.
  (ligature-set-ligatures 't '("www"))

  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it.
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))

  ;; Source: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions#using-ligatureel
  ;; Enable ligatures in programming modes.
  (ligature-set-ligatures
   '(clojure-mode dhall-mode haskell-mode)
   '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
     ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
     "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
     "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" "/*" "/**"
     "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
     "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
     "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
     "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
     "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
     "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

  (global-ligature-mode t))

(provide 'init-font)

;;; init-font.el ends here
