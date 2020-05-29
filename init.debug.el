(setq load-prefer-newer t)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(toggle-scroll-bar -1)
(menu-bar-mode -1)
(package-initialize)
(setq scroll-bar-background nil)

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

(straight-use-package 'evil)
(straight-use-package 'evil-matchit)
(straight-use-package 'js2-mode)
(straight-use-package 'rjsx-mode)
(straight-use-package 'typescript-mode)

(evil-mode 1)
(global-evil-matchit-mode 1)
(find-file (expand-file-name "~/Downloads/test.js"))