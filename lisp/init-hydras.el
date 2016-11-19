(global-unset-key (kbd "<f1>"))

(global-set-key (kbd "<f1>") 'jco/hydra-main-menu/body)

(defun open-config-file (file-name)
  (interactive)
  (find-file (concat user-emacs-directory file-name)))

(defhydra jco/hydra-main-menu (:color blue :hint nil)
  "
Menu: _a_propos _c_fg _h_g st _m_ail _p_ackages"
  ("a" jco/hydra-apropos/body)
  ("c" jco/hydra-edit-config/body)
  ("h" monky-status)
  ("m" mu4e)
  ("p" jco/hydra-packages/body))

(defhydra jco/hydra-edit-config (:color blue :hint nil)
  "
Edit cfg: _i_nit _c_ommon _h_ydras _p_ackages _t_heme"
  ("i" (open-config-file "init.el"))
  ("c" (open-config-file "lisp/init-common.el"))
  ("h" (open-config-file "lisp/init-hydras.el"))
  ("p" (open-config-file "lisp/init-packages.el"))
  ("t" (open-config-file "lisp/init-theme.el")))

(defhydra jco/hydra-apropos (:color blue :hint nil)
  "
Apropos: _a_propos _c_md _d_oc _v_al _l_ib _o_ption _v_ar _i_nfo _t_ags"
  ("a" apropos)
  ("c" apropos-command)
  ("d" apropos-documentation)
  ("e" apropos-value)
  ("l" apropos-library)
  ("o" apropos-user-option)
  ("v" apropos-variable)
  ("i" info-apropos)
  ("t" tags-apropos))

(defhydra jco/hydra-packages (:color blue :hint nil)
  "
Packages: _l_ist _n_o-fetch _u_pgrade-all
"
  ("l" list-packages)
  ("n" package-list-packages-no-fetch)
  ("u" package-utils-upgrade-all))

(provide 'init-hydras)
