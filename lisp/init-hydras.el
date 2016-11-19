(global-set-key (kbd "<f1>") 'jco/hydra-main-menu/body)

(defun open-config-file (file-name)
  (interactive)
  (find-file (concat user-emacs-directory file-name)))

(defhydra jco/hydra-main-menu (:color blue :hint nil)
  "
Menu: _a_propos _e_dit-cfg _m_ail _p_kgs _s_nippets s_w_oop _v_cs"
  ("a" jco/hydra-apropos/body)
  ("e" jco/hydra-edit-config/body)
  ("m" mu4e)
  ("p" jco/hydra-packages/body)
  ("s" jco/hydra-snippets/body)
  ("w" jco/hydra-swoop/body)
  ("v" jco/hydra-vcs/body))

(defhydra jco/hydra-edit-config (:color blue :hint nil)
  "
Edit cfg: _i_nit _c_ommon _f_ile _h_ydras _p_ackages _t_heme"
  ("i" (open-config-file "init.el"))
  ("c" (open-config-file "lisp/init-common.el"))
  ("f" (helm-find-files-1 (expand-file-name "~/.emacs.d/lisp/")))
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
Packages: _l_ist _n_o-fetch _u_pgrade-all"
  ("l" list-packages)
  ("n" package-list-packages-no-fetch)
  ("u" package-utils-upgrade-all))

(defhydra jco/hydra-snippets (:color blue :hint nil)
  "
Snippets: _i_nsert _e_dit _r_eload"
  ("i" yas/insert-snippet)
  ("e" yas/visit-snippet-file)
  ("r" yas/reload-all))

(defhydra jco/hydra-swoop (:color blue)
  "Swoop:"
  ("m" helm-multi-swoop "multi")
  ("M" helm-multi-swoop-all "multi-all")
  ("s" helm-swoop "swoop"))

(defhydra jco/hydra-vcs (:color blue :hint nil)
  "
VCS: _g_it _m_ercurial"
  ("g" magit-status)
  ("m" monky-status))

(provide 'init-hydras)
