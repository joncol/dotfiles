(evil-leader/set-key "m" 'jco/hydra-main-menu/body)

(global-set-key (kbd "<f1>") 'jco/hydra-help/body)

(defun open-config-file (file-name)
  (interactive)
  (find-file (concat user-emacs-directory file-name)))

(defhydra jco/hydra-main-menu (:color teal :hint nil)
  "
Menu: _a_pp _e_dit-cfg _p_kgs _s_woop _S_nippets _v_cs"
  ("a" jco/hydra-app/body)
  ("e" jco/hydra-edit-config/body)
  ("p" jco/hydra-packages/body)
  ("s" jco/hydra-swoop/body)
  ("S" jco/hydra-snippets/body)
  ("v" jco/hydra-vcs/body))

(defhydra jco/hydra-edit-config (:color teal :hint nil)
  "
Edit cfg: _i_nit _c_ommon _f_ile _h_ydras _p_ackages _t_heme"
  ("i" (open-config-file "init.el"))
  ("c" (open-config-file "lisp/init-common.el"))
  ("f" (helm-find-files-1 (expand-file-name "~/.emacs.d/lisp/")))
  ("h" (open-config-file "lisp/init-hydras.el"))
  ("p" (open-config-file "lisp/init-packages.el"))
  ("t" (open-config-file "lisp/init-theme.el")))

(defhydra jco/hydra-packages (:color teal :hint nil)
  "
Packages: _l_ist _n_o-fetch _u_pgrade-all"
  ("l" list-packages)
  ("n" package-list-packages-no-fetch)
  ("u" package-utils-upgrade-all))

(defhydra jco/hydra-snippets (:color teal :hint nil)
  "
Snippets: _i_nsert _e_dit _r_eload"
  ("i" yas/insert-snippet)
  ("e" yas/visit-snippet-file)
  ("r" yas/reload-all))

(defhydra jco/hydra-swoop (:color teal :hint nil)
  "
Swoop: _m_ulti multi-_a_ll _s_woop"
  ("m" helm-multi-swoop)
  ("a" helm-multi-swoop-all)
  ("s" helm-swoop))

(defhydra jco/hydra-vcs (:color teal :hint nil)
  "
VCS: _g_it _m_ercurial"
  ("g" magit-status)
  ("m" monky-status))

(defhydra jco/hydra-app (:color teal :hint nil)
  "
App: _e_rc _m_u4e"
  ("e" (erc :server "irc.freenode.net" :port 6667))
  ("m" mu4e))

;;; Help menu

(defhydra jco/hydra-help (:color teal :hint nil)
  "
_a_propos"
  ("a" jco/hydra-apropos/body))

(defhydra jco/hydra-apropos (:color teal :hint nil)
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

(provide 'init-hydras)
