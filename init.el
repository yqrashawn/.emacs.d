(defvar yq-emacs-cache-dir (concat user-emacs-directory ".cache/"))
(defvar yq-emacs-dotfile-dir (concat user-emacs-directory "init.el"))

(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(defun yq/get-modules (module-dir)
  (concat user-emacs-directory "modules/" module-dir))

(load-file (yq/get-modules "evil-core.el" ))
(load-file (yq/get-modules "better-default.el" ))
(load-file (yq/get-modules "swiper.el" ))
(load-file (yq/get-modules "edit.el" ))
(load-file (yq/get-modules "version-control.el" ))
(load-file (yq/get-modules "prog.el" ))
(load-file (yq/get-modules "dev.el"))
(load-file (yq/get-modules "visual.el"))
(load-file (yq/get-modules "lang.el"))

;; TODO: better defaults
