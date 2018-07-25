;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (message "Emacs ready in %s with %d garbage collections."
;;                      (format "%.2f seconds"
;;                              (float-time
;;                               (time-subtract after-init-time before-init-time)))
;;                      gcs-done)))
;; Always load newest byte code
(setq load-prefer-newer t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(toggle-scroll-bar -1)
(menu-bar-mode -1)
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
(setq gc-cons-threshold 100000000)

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

;; use the under vc straight/repos dir instead of straight/build one
;; (defun yq/straight-library-build-to-repo (library)
;;   (interactive (list (read-library-name)))
;;   (let* ((lib-path (find-library-name library))
;;          (right-lib-path (or (and (string-match "straight/build" lib-path) (replace-match "straight/repos" nil nil lib-path)) lib-path)))
;;     (prog1
;;         (switch-to-buffer (find-file-noselect right-lib-path))
;;       (run-hooks 'find-function-after-hook))))
;; (advice-add 'find-library :override 'yq/straight-library-build-to-repo)

(straight-use-package 'diminish)
(straight-use-package 'use-package)

(use-package exec-path-from-shell
  :straight t
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
(yq/get-modules "org-agenda.el")
(yq/get-modules "auto-detect-which-machine.el")
(yq/get-modules "mail.el")
(yq/get-modules "pdf.el")

(use-package server)
(unless (server-running-p) (server-start))

;; TODO noderepl
(use-package playground
  :straight (:host github :repo "akirak/emacs-playground"))

(setq gc-cons-threshold 8388608)
(toggle-frame-maximized)
;; (straight-use-package 'suggest)
;; (straight-use-package 'edit-list)
;; (use-package zpresent)

(global-set-key (kbd "s-?") 'info-display-manual)
