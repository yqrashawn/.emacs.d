;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (message "Emacs ready in %s with %d garbage collections."
;;                      (format "%.2f seconds"
;;                              (float-time
;;                               (time-subtract after-init-time before-init-time)))
;;                      gcs-done)))
;; Always load newest byte code
;; (setq load-prefer-newer t)
(setq debug-on-error t)
(setq debug-on-quit t)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(toggle-scroll-bar -1)
(menu-bar-mode -1)
(setq straight-check-for-modifications 'live-with-find)
;; (package-initialize)
(setq scroll-bar-background nil)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . 'dark)) ; or 'dark, to switch to white title text
(defvar yq-emacs-cache-dir (concat user-emacs-directory ".cache/"))
(defvar spacemacs-cache-directory (concat user-emacs-directory ".cache/"))
(defvar yq-emacs-dotfile-dir (concat user-emacs-directory "init.el"))
(setq url-configuration-directory (concat spacemacs-cache-directory "url/"))
(setq custom-file (concat yq-emacs-cache-dir ".custom-settings"))
(load-file custom-file)

(setq url-proxy-services
      '(("http" . "127.0.0.1:6152")
        ("https" . "127.0.0.1:6152")))

(setq gc-cons-threshold 8388608)

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold (* 8 1024 1024)))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;;; Temporarily disable the file name handler.
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun ambrevar/reset-file-name-handler-alist ()
  (setq file-name-handler-alist default-file-name-handler-alist))
(add-hook 'after-init-hook #'ambrevar/reset-file-name-handler-alist)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'diminish)
(setq use-package-enable-imenu-support t)
(straight-use-package 'use-package)

(defmacro defip (name &rest body)
  (declare (indent 1) (debug t))
  `(defun ,name (&optional _arg)
     ,(if (stringp (car body)) (car body))
     (interactive "p")
     ,@(if (stringp (car body)) (cdr `,body) body)))

(defmacro def (&rest body)
  (declare (indent 1) (debug t))
  `(lambda ()
     (interactive)
     ,@body))

(defmacro add-lam (hook &rest body)
  (declare (indent 1) (debug t))
  `(add-hook ,hook (lambda () ,@body)))

(defmacro use-feature (name &rest args)
  (declare (indent 1))
  `(use-package ,name
     :straight nil
     ,@args))

(use-package exec-path-from-shell
  :straight t
  :init
  (setq exec-path-from-shell-arguments '("-l"))
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(defun yq/get-modules (module-dir)
  (let* ((el-file-path (concat user-emacs-directory "modules/" module-dir))
         (elc-file-path (concat el-file-path "c")))
    (if (file-exists-p elc-file-path)
        (load-file elc-file-path)
      (load-file el-file-path))))

(yq/get-modules "evil-core.el")
(yq/get-modules "better-default.el")
(yq/get-modules "navigation.el")
(yq/get-modules "edit.el")
(yq/get-modules "version-control.el")
(yq/get-modules "prog.el")
(yq/get-modules "dev.el")
(yq/get-modules "visual.el")
(yq/get-modules "osx.el")
(yq/get-modules "shell.el")
(yq/get-modules "lang.el")
(yq/get-modules "comm-funcs.el")
(yq/get-modules "auto-detect-which-machine.el")
(yq/get-modules "mail.el")
;; (yq/get-modules "pdf.el")

(use-package server)
(unless (server-running-p) (server-start))

;; (use-package playground
;;   :straight (:host github :repo "akirak/emacs-playground"))


;; (toggle-frame-maximized)

(use-package suggest
  :straight t
  :commands (suggest))

(use-package which-key
  :straight t
  :commands (which-key-mode))

(use-package ix
  :straight t
  :commands (ix))

;; (use-package zpresent)

(global-set-key (kbd "M-0") 'delete-frame)

(setq debug-on-error nil)
(setq debug-on-quit nil)
