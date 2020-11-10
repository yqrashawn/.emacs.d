;;; init.el --- init scripts                   -*- lexical-binding: t; -*-

;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (message "Emacs ready in %s with %d garbage collections."
;;                      (format "%.2f seconds"
;;                              (float-time
;;                               (time-subtract after-init-time before-init-time)))
;;                      gcs-done)))
;; Always load newest byte code
(setq load-prefer-newer t)
(setq debug-on-error t)
(setq debug-on-quit t)

;; Remove the built-in version of Org from the load-path
(require 'cl-seq)
(setq load-path (cl-remove-if (lambda (x) (string-match-p "org$" x)) load-path))
(setq load-path (cl-remove-if (lambda (x) (string-match-p "org-20" x)) load-path))

(add-to-list 'load-path "~/org-mode/lisp/")
(add-to-list 'load-path "~/org-mode/contrib/lisp/")

;; check package update infos
(setq straight-vc-git-auto-fast-forward t)
;; (setq straight-fix-org t)
;; (package-initialize)
(setq scroll-bar-background nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; macos transparent titlebar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; dark titlebar
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Fullscreen by default, as early as possible. use yabai
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; UTF-8 everywhere, please.
(prefer-coding-system 'utf-8)

(defvar yq-emacs-cache-dir (concat user-emacs-directory ".cache/"))
(defvar spacemacs-cache-directory (concat user-emacs-directory ".cache/"))
(defvar yq-emacs-dotfile-dir (concat user-emacs-directory "init.el"))
(setq url-configuration-directory (concat spacemacs-cache-directory "url/"))
(setq custom-file (concat yq-emacs-cache-dir ".custom-settings"))
(load custom-file)

;; (setq url-proxy-services
;;       '(("http" . "127.0.0.1:8888")
;;         ("https" . "127.0.0.1:8889")))

;;; Temporarily disable the file name handler.
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun ambrevar/reset-file-name-handler-alist ()
  (setq file-name-handler-alist
        (append default-file-name-handler-alist
                file-name-handler-alist))
  (cl-delete-duplicates file-name-handler-alist :test 'equal))
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


;; (straight-use-package 'auto-compile)
;; (auto-compile-on-save-mode 1)
;; (auto-compile-on-load-mode 1)

(straight-use-package 'diminish)

;; Allow navigation between use-package stanzas with iMenu.
(setq-default use-package-enable-imenu-support t)
(straight-use-package 'use-package)
(straight-use-package 'use-package-ensure-system-package)
(use-package benchmark-init
  :straight t
  :disabled
  :hook (after-init . benchmark-init/dactivate)
  :init
  (benchmark-init/activate))

(defmacro def (name &rest body)
  (declare (indent 1) (debug t))
  `(defun ,name (&optional _arg)
     ,(if (stringp (car body)) (car body))
     (interactive "p")
     ,@(if (stringp (car body)) (cdr `,body) body)))

(defmacro defl (&rest body)
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
  (defun yq/update-path ()
    (dolist (dir
             (list
              (expand-file-name "~/local/bin")
              (expand-file-name "/Applications/Emacs.app/Contents/MacOS/bin")
              (expand-file-name "~/.fnm/current/bin")))
      (when (and (file-exists-p dir) (not (member dir exec-path)))
        (setenv "PATH" (concat dir ":" (getenv "PATH")))
        (setq exec-path (append (list dir) exec-path)))))
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "JAVA_HOME")
    (yq/update-path)))

(use-package el-patch
  :straight t)

(eval-when-compile
  (require 'el-patch))

(defun yq/get-modules (module-dir)
  (let* ((el-file-path (concat user-emacs-directory "modules/" module-dir))
         (elc-file-path (concat el-file-path "c")))
    (if (file-exists-p elc-file-path)
        (load-file elc-file-path)
      (load-file el-file-path))))

(yq/get-modules "core-display-init.el")
(yq/get-modules "evil-core.el")
(yq/get-modules "in-terminal.el")
(yq/get-modules "better-default.el")
(yq/get-modules "navigation.el")
(yq/get-modules "edit.el")
(yq/get-modules "lang/org2.el")
(yq/get-modules "version-control.el")
(yq/get-modules "prog.el")
(yq/get-modules "dev.el")
(yq/get-modules "osx.el")
(yq/get-modules "shell.el")
(yq/get-modules "lang.el")
(yq/get-modules "comm-funcs.el")
;; (yq/get-modules "mail.el")
(yq/get-modules "visual.el")
(yq/get-modules "auto-detect-which-machine.el")
;; (yq/get-modules "pdf.el")
(yq/get-modules "slack.el")

(use-package server
  :init
  (unless (server-running-p) (server-start)))

(use-package playground
  :disabled
  :straight (:host github :repo "akirak/emacs-playground"))

;; (toggle-frame-maximized)

(use-package suggest
  :straight t
  :commands (suggest))

(use-package which-key
  :straight t
  :commands (which-key-mode))

(use-package keycast
  :straight t
  :commands (keycast-mode))

(use-package command-log-mode
  :straight t
  :commands (global-command-log-mode))

;; (use-package zpresent)

(global-set-key (kbd "M-0") 'delete-frame)

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold (* 128 1024 1024)))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(setq gc-cons-percentage 0.1)
(defvar gc-timer nil)
(defun maybe-gc ()
  (let ((original gc-cons-threshold))
    (setq gc-cons-threshold 800000)
    (setq gc-cons-threshold original
          gc-timer (run-with-timer 2 nil #'schedule-maybe-gc))))

(defun schedule-maybe-gc ()
  (setq gc-timer (run-with-idle-timer 2 nil #'maybe-gc)))

(schedule-maybe-gc)

(setq debug-on-error nil)
(setq debug-on-quit nil)

;; Tell straight.el about the profiles we are going to be using.
(setq straight-profiles
      '((nil . "default.el")
        ;; Packages which are pinned to a specific commit.
        (pinned . "pinned.el")))

(autoload #'straight-x-pull-all "straight-x")
(autoload #'straight-x-freeze-versions "straight-x")

;; (let ((straight-current-profile 'pinned))
;;   (setq straight-x-pinned-packages
;;         '(
;;           ("doom-modeline" . "e6d690bae01cb68e7171857fe07ac914d7a19f4b"))))


;; tmp fix
;; https://emacs.stackexchange.com/questions/5552/emacs-on-android-org-mode-error-wrong-type-argument-stringp-require-t
;; https://www.reddit.com/r/emacs/comments/bezim2/issue_with_withevalafterload_and_emacs_27/
(defun load-history-filename-element (file-regexp)
  "Get the first elt of `load-history' whose car matches FILE-REGEXP.
        Return nil if there isn't one."
  (let* ((loads load-history)
         (load-elt (and loads (car loads))))
    (save-match-data
      (while (and loads
                  (or (null (car load-elt))
                      (not (and (stringp (car load-elt)) ; new condition
                                (string-match file-regexp (car load-elt))))))
        (setq loads (cdr loads)
              load-elt (and loads (car loads)))))
    load-elt))
