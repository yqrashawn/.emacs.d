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

;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/org") ;; with elc
(add-to-list 'load-path (expand-file-name "~/org-mode/lisp/"))
;; (add-to-list 'load-path "~/org-mode/contrib/lisp/")

;; check package update infos
(setq straight-vc-git-auto-fast-forward t)
(setq straight-fix-org t)
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

(setq straight-check-for-modifications '(check-on-save find-when-checking))
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

(defun in-terminal-p ()
 (or (not (display-graphic-p)) (daemonp)))

(defun nin-terminal-p ()
  (and (display-graphic-p) (not (daemonp))))


;; (straight-use-package 'auto-compile)
;; (auto-compile-on-save-mode 1)
;; (auto-compile-on-load-mode 1)

(straight-use-package 'diminish)

;; Allow navigation between use-package stanzas with iMenu.
(setq-default use-package-enable-imenu-support t)
(straight-use-package 'use-package)

(use-package org
  :straight (:local-repo "~/org-mode/" :no-build t)
  :defer t)

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

(setq-default comp-async-report-warnings-errors nil)

(setq explicit-shell-file-name "/usr/local/bin/zsh")
(setq shell-file-name "/usr/local/bin/zsh")

(defun yq/get-shell-env ()
  (split-string (replace-regexp-in-string "\n$" "" (shell-command-to-string "awk 'BEGIN{for(v in ENVIRON) print v}'")) "\n"))

(use-package exec-path-from-shell
  :straight t
  :commands (exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-arguments '("-l" "-i"))
  (exec-path-from-shell-variables '("GOPATH" "FNM_NODE_DIST_MIRROR" "NODE_REPL_MODE" "HOMEBREW_REPOSITORY" "COLORTERM" "FZF_DEFAULT_COMMAND" "STARSHIP_SHELL" "NODE_REPL_HISTORY_SIZE" "GIT_EDITOR" "LOGNAME" "GREP_COLOR" "EMAIL" "LSCOLORS" "ANDROID_HOME" "FZF_CTRL_T_COMMAND" "FNM_DIR" "HOMEBREW_PREFIX" "MANPAGER" "FNM_LOGLEVEL" "XPC_SERVICE_NAME" "JAVA_HOME" "JABBA_HOME" "USER" "PYTHONIOENCODING" "JAVA_HOME_BEFORE_JABBA" "LANG" "PATH" "ANDROID_SDK_ROOT" "TERM" "TMPDIR" "LC_ALL" "HOMEBREW_CELLAR" "HOME" "SSH_KEY_PATH" "ROAMER_EDITOR" "LESS" "LS_COLORS" "PYENV_ROOT" "NODE_REPL_HISTORY" "SHELL" "USE_EDITOR" "FNM_MULTISHELL_PATH" "GEM_PATH" "LC_CTYPE" "YQ_MACHINE" "XDG_CONFIG_HOME" "RUBY_CONFIGURE_OPTS" "RBENV_SHELL" "GOROOT" "BROWSER" "GEM_HOME" "EDITOR" "SSH_AUTH_SOCK" "PAGER" "GOBIN" "VISUAL"))
  :init
  (exec-path-from-shell-initialize)
  (defun yq/update-path ()
    (interactive)
    (dolist (dir
             (list
              (expand-file-name "~/local/bin")
              (expand-file-name "/Applications/Emacs.app/Contents/MacOS/bin")))
      (when (and (file-exists-p dir) (not (member dir exec-path)))
        (setenv "PATH" (concat dir ":" (getenv "PATH")))
        (setq exec-path (append (list dir) exec-path)))))
  (when (memq window-system '(mac ns x))
    (run-with-idle-timer 60 t (defl (exec-path-from-shell-initialize)))))

(use-package el-patch :straight t)

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
;; (yq/get-modules "slack.el")

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

(defun +setup-gc ()
  (setq gc-cons-percentage 0.6)
  (setq gc-cons-threshold (* 8 (expt 10 8))))
(+setup-gc)

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  (+setup-gc))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)


(setq jit-lock-contextually 'syntax-driven
      jit-lock-context-time 2.0
      jit-lock-stealth-nice 0.25
      jit-lock-antiblink-grace 1
      jit-lock-chunk-size 1000
      jit-lock-defer-time 0.25
      jit-lock-stealth-time 0.25
      jit-lock-stealth-load 300)

;; (setq font-lock-maximum-decoration nil)

(defvar gc-timer nil)

(defun maybe-gc ()
  (let ((original gc-cons-threshold))
    (+setup-gc)
    (setq gc-timer (run-with-timer 2 nil #'schedule-maybe-gc))))

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

(setq org-version "9.4.4")
