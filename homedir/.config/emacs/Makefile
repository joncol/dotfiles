.PHONY: profile
profile:
	emacs -Q -l lisp/profile-dotemacs.el \
	--eval "(setq profile-dotemacs-file \
	(setq load-file-name \"$(abspath init.el)\"))" \
	-f profile-dotemacs