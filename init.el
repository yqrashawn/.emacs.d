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

;; C-h key as BS
(keyboard-translate ?\C-h ?\C-?)
(global-set-key [(control ?h)] 'delete-backward-char)

(straight-use-package 'use-package)

(toggle-debug-on-error)
(add-hook 'prog-mode-hook 'hs-minor-mode)
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

(defun yq/kill-this-buffer (&optional arg)
  "Kill the current buffer.
If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal '(4) arg)
        (kill-buffer-and-window)
      (kill-buffer))))

(defun yq/delete-window (&optional arg)
  "Delete the current window.
If the universal prefix argument is used then kill the buffer too."
  (interactive "P")
  (if (equal '(4) arg)
      (kill-buffer-and-window)
    (delete-window)))

(defun yq/swiper-region-or-symbol ()
  "Run `swiper' with the selected region or the symbol
around point as the initial input."
  (interactive)
  (let ((input (if (region-active-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))
                 (thing-at-point 'symbol t))))
    (swiper input)))


(defun yq/backward-kill-word-or-region (&optional arg)
  "Calls `kill-region' when a region is active and
`backward-kill-word' otherwise. ARG is passed to
`backward-kill-word' if no region is active."
  (interactive "p")
  (if (region-active-p)
      ;; call interactively so kill-region handles rectangular selection
      ;; correctly (see https://github.com/syl20bnr/spacemacs/issues/3278)
      (call-interactively #'kill-region)
    (backward-kill-word arg)))

(global-set-key (kbd "C-w") 'yq/backward-kill-word-or-region)

(use-package mwim
  :straight t
  :config
  (global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (global-set-key (kbd "C-e") 'mwim-end-of-code-or-line))

(use-package swiper
  :straight t
  :config
  (global-set-key (kbd "C-SPC") 'counsel-grep-or-swiper)
  (global-set-key (kbd "^@") 'counsel-grep-or-swiper)
  (global-set-key (kbd "C-S-SPC") 'yq/swiper-region-or-symbol))

(use-package evil-leader
  :straight t
  :init
  (setq evil-leader/in-all-states t)
  :config
  (evil-leader/set-leader "<SPC>" "M-")
  (global-evil-leader-mode))

(use-package evil
  :straight t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-Y-yank-to-eol t)
  :config
  (define-key evil-insert-state-map "zl" 'hs-hide-level)
  (define-key evil-normal-state-map "s" nil)
  (define-key evil-normal-state-map "sk" 'yq/kill-this-buffer)
  (define-key evil-normal-state-map "sc" 'yq/delete-window)
  (define-key evil-normal-state-map "sh" 'save-buffer)
  (define-key evil-normal-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
  (evil-leader/set-key "w" nil)
  (evil-leader/set-key "wh" 'evil-window-left)
  (evil-leader/set-key "wj" 'evil-window-down)
  (evil-leader/set-key "wk" 'evil-window-up)
  (evil-leader/set-key "wl" 'evil-window-right)
  (evil-mode 1))


;; ;; ( evil-set-initial-state MODE STATE)

(use-package magit
  :straight t
  :config
  (evil-leader/set-key "g" nil)
  (evil-leader/set-key "gs" 'magit-status))

(use-package counsel
  :straight t
  :config
  (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (evil-leader/set-key "<SPC>" 'counsel-M-x)
  (evil-leader/set-key "f" nil)
  (evil-leader/set-key "ff" 'counsel-find-file)
  (evil-leader/set-key "h" nil)
  (evil-leader/set-key "hd" nil)
  (evil-leader/set-key "hdf" 'counsel-describe-function)
  (evil-leader/set-key "hdv" 'counsel-describe-variable)
  (evil-leader/set-key "hdk" 'describe-key)
  (evil-leader/set-key "hdh" 'counsel-describe-symbol-history)
  (define-key evil-normal-state-map "sj" 'counsel-recentf))

(use-package iedit
  :straight t)

(use-package evil-iedit-state
  :straight t)

(use-package evil-magit
  :straight t)

(use-package evil-args
  :straight t
  :config ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

(use-package mode-line-bell
  :straight t
  :config (mode-line-bell-mode))

;; bind evil-forward/backward-args
;; (define-key evil-normal-state-map "L" 'evil-forward-arg)
;; (define-key evil-normal-state-map "H" 'evil-backward-arg)
;; (define-key evil-motion-state-map "L" 'evil-forward-arg)
;; (define-key evil-motion-state-map "H" 'evil-backward-arg)
;; bind evil-jump-out-args
;; (define-key evil-normal-state-map "K" 'evil-jump-out-args)

(use-package expand-region
  :straight t
  :config
  (define-key evil-normal-state-map "sv" 'er/expand-region)
  (setq expand-region-contract-fast-key "V"
        expand-region-reset-fast-key "r"))

(use-package evil-search-highlight-persist
  :straight t
  :config
  (global-evil-search-highlight-persist t)
  (setq evil-search-highlight-string-min-len 1
        evil-search-highlight-persist-all-windows t))
