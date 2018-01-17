(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "http")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://elpa.emacs-china.org/melpa/")) t)
  (add-to-list 'package-archives (cons "org-elpa" (concat proto "://elpa.emacs-china.org/org/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("elpa" . (concat proto "://elpa.emacs-china.org/gnu/")))))
(package-initialize)

(setq url-proxy-services
      '(("http" . "127.0.0.1:6152")
        ("https" . "127.0.0.1:6152")))

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

(use-package magit
  :straight t)

(use-package swiper
  :straight t)
(use-package ivy
  :straight t)
(use-package flycheck
  :straight t)
(use-package git-commit
  :straight t)
(use-package company
  :straight t)
(use-package magit-popup
  :straight t)
(use-package avy
  :straight t)
(use-package bind-key
  :straight t)
(use-package iedit
  :straight t)

(use-package undo-tree
  :straight (:host github :repo "emacsmirror/undo-tree")
  :config (global-undo-tree-mode))


(use-package goto-chg
  :straight (:host github :repo "emacs-evil/goto-chg"))

(use-package evil
  :straight (:host github :repo "emacs-evil/evil")
  :config (evil-mode 1))