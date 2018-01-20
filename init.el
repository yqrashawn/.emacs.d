;; (require 'package)
(defvar yq-emacs-cache-dir (concat user-emacs-directory ".cache/"))
(defvar spacemacs-cache-directory (concat user-emacs-directory ".cache/"))
(defvar yq-emacs-dotfile-dir (concat user-emacs-directory "init.el"))
(setq url-configuration-directory (concat spacemacs-cache-directory "url/"))
(setq custom-file (concat yq-emacs-cache-dir ".custom-settings"))
(load-file custom-file)


(setq url-proxy-services
      '(("http" . "127.0.0.1:6152")
        ("https" . "127.0.0.1:6152")))
(setq gc-cons-threshold 100000000)
;; (package-initialize)

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold (* 8 1024 1024)))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

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

(straight-use-package 'diminish)
(straight-use-package 'use-package)

(defun yq/get-modules (module-dir)
  (load-file (concat user-emacs-directory "modules/" module-dir)))

(yq/get-modules "evil-core.el")
(yq/get-modules "better-default.el")
(yq/get-modules "swiper.el")
(yq/get-modules "edit.el")
(yq/get-modules "version-control.el")
(yq/get-modules "prog.el")
(yq/get-modules "dev.el")
(yq/get-modules "visual.el")
(yq/get-modules "osx.el")
(yq/get-modules "lang.el")

(use-package server)
(unless (server-running-p) (server-start))
(setq gc-cons-threshold 8388608)

;;TODO popwin tide react vue ediff lispy org noderepl js default hl-todo
;;TODO typescript web-mode rust
;;TODO restart-emacs
