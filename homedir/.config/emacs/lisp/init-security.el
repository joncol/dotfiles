;;; #init-security.el --- Security config -*- lexical-binding: t; -*-

;;; Commentary:

;; This code is from
;; https://glyph.twistedmatrix.com/2015/11/editor-malware.html.

;; It requires pip package "certifi" (pip install certifi), and gnutls-cli.
;; On Debian, gnutls can be installed by `apt-get install gnutls-bin'.
;; On Windows, download binaries and put in emacs/bin directory:
;; https://ftp.heanet.ie/mirrors/ftp.gnupg.org/gcrypt/gnutls/w32/

;;; Code:

(if (version< emacs-version "27")
    (progn (require 'tls)
           (setq tls-checktrust 'ask)

           (let ((trustfile (replace-regexp-in-string
                             "\\\\" "/"
                             (replace-regexp-in-string
                              "\n" ""
                              (shell-command-to-string "python -m certifi")))))
             (setq tls-program (list
                                (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                                        (if (eq window-system 'w32) ".exe" "") trustfile)))

             (setq gnutls-verify-error t)
             (setq gnutls-trustfiles (list trustfile))))
  (setq epg-pinentry-mode 'loopback))

(provide 'init-security)

;;; init-security.el ends here
