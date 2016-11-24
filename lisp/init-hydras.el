(evil-leader/set-key "m" 'jco/hydra-main-menu/body)

(global-set-key (kbd "<f1>") 'jco/hydra-help/body)

(defun open-config-file (file-name)
  (interactive)
  (find-file (concat user-emacs-directory file-name)))

(defhydra jco/hydra-main-menu (:color teal :hint nil)
  "
menu: _a_pp _b_ookmarks _c_fg _f_ind _l_ang _o_rg _p_kgs _s_woop _S_nippets _v_cs"
  ("a" jco/hydra-app/body)
  ("b" helm-bookmarks)
  ("c" jco/hydra-config/body)
  ("f" jco/hydra-find/body)
  ("l" jco/hydra-lang/body)
  ("o" jco/hydra-org/body)
  ("p" jco/hydra-packages/body)
  ("s" jco/hydra-swoop/body)
  ("S" jco/hydra-snippets/body)
  ("v" jco/hydra-vcs/body))

(defhydra jco/hydra-config (:color teal :hint nil)
  "
edit cfg: _i_nit _b_ootstrap _c_ommon _f_ile _h_ydras _p_ackages _t_heme _u_pdate"
  ("i" (open-config-file "init.el"))
  ("b" (open-config-file "lisp/init-bootstrap.el"))
  ("c" (open-config-file "lisp/init-common.el"))
  ("f" (helm-find-files-1 (expand-file-name "~/.emacs.d/lisp/")))
  ("h" (open-config-file "lisp/init-hydras.el"))
  ("p" (open-config-file "lisp/init-packages.el"))
  ("t" (open-config-file "lisp/init-theme.el"))
  ("u" (jco/update-config)))

(defhydra jco/hydra-find (:color teal :hint nil)
  "
find: _f_un _l_ib _v_ar"
  ("f" find-function)
  ("l" find-library)
  ("v" find-variable))

(defhydra jco/hydra-lang (:color teal :hint nil)
"
_f_lyspell

_l_angtool _c_orrect _d_one"
  ("f" flyspell-mode)
  ("l" langtool-check)
  ("c" langtool-correct-buffer)
  ("d" langtool-check-done))

(defhydra jco/hydra-org (:color teal :hint nil)
  "
Org: _a_genda _p_omodoro"
  ("a" (org-agenda nil "d"))
  ("p" (org-pomodoro)))

(defhydra jco/hydra-packages (:color teal :hint nil)
  "
packages: _l_ist _n_o-fetch _u_pgrade-all"
  ("l" list-packages)
  ("n" package-list-packages-no-fetch)
  ("u" package-utils-upgrade-all))

(defhydra jco/hydra-snippets (:color teal :hint nil)
  "
snippets: _i_nsert _e_dit _r_eload"
  ("i" yas/insert-snippet)
  ("e" yas/visit-snippet-file)
  ("r" yas/reload-all))

(defhydra jco/hydra-swoop (:color teal :hint nil)
  "
swoop: _m_ulti multi-_a_ll _s_woop"
  ("m" helm-multi-swoop)
  ("a" helm-multi-swoop-all)
  ("s" helm-swoop))

(defhydra jco/hydra-vcs (:color teal :hint nil)
  "
vcs: _g_it _m_ercurial"
  ("g" magit-status)
  ("m" monky-status))

(defhydra jco/hydra-app (:color teal :hint nil)
  "
app: _c_irce _e_rc _m_u4e e_s_hell _v_im e_w_w s_x_"
  ("c" (circe "Freenode"))
  ("e" (erc :server "irc.freenode.net" :port 6667))
  ("m" mu4e)
  ("s" jco/eshell-here)
  ("v" vim)
  ("w" eww)
  ("x" sx-tab-all-questions))

;;; Help menu

(defhydra jco/hydra-help (:color teal :hint nil)
  "
help: _a_propos"
  ("a" jco/hydra-apropos/body))

(defhydra jco/hydra-apropos (:color teal :hint nil)
  "
apropos: _a_propos _c_md _d_oc _v_al _l_ib _o_ption _v_ar _i_nfo _t_ags"
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
