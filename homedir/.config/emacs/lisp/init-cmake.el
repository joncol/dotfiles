;;; #init-cmake.el --- CMake configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(use-package cmake-mode
  :init
  (add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
  :config
  (setq cmake-tab-width 4))

(provide 'init-cmake)

;;; init-cmake.el ends here
