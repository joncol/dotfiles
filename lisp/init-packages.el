;;; #init-packages.el --- Install used packages -*- lexical-binding: t; -*-

;;; Commentary:

;; Takes care of installing all used packages from package archives.

;;; Code:

(require 'init-security)

(require 'package)
(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(setq use-package-always-ensure t)

(provide 'init-packages)

;;; init-packages.el ends here
