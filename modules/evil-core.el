(yq/get-modules "evil-core-funcs.el")

(use-package undo-tree
  :straight (:host github :repo "emacsmirror/undo-tree")
  :config (global-undo-tree-mode))

(use-package goto-chg
  :straight (:host github :repo "emacs-evil/goto-chg"))

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
  (setq evil-move-cursor-back nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-symbol-word-search t)
  (setq evil-kill-on-visual-paste nil)
  (setq evil-esc-delay 0)
  (setq evil-shift-width 2)
  (setq evil-show-paren-range 1)
  :config
  (define-key evil-normal-state-map "zl" 'hs-hide-level)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-toggle-fold)
  (define-key evil-normal-state-map "s" nil)
  (define-key evil-normal-state-map "sk" 'yq/kill-this-buffer)
  (define-key evil-normal-state-map "sc" 'yq/delete-window)
  (define-key evil-normal-state-map "sh" 'save-buffer)
  (define-key evil-normal-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
  (define-key evil-normal-state-map (kbd "C-m") 'evil-jump-item)
  (define-key evil-visual-state-map (kbd "C-m") 'evil-jump-item)
  (define-key evil-insert-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
  (define-key evil-insert-state-map (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (define-key evil-visual-state-map (kbd "C-e") 'evil-end-of-line)
  (define-key evil-visual-state-map (kbd "C-a") 'evil-first-non-blank)
  (evil-leader/set-key "w" nil)
  (evil-leader/set-key "wh" 'evil-window-left)
  (evil-leader/set-key "wj" 'evil-window-down)
  (evil-leader/set-key "wk" 'evil-window-up)
  (evil-leader/set-key "wl" 'evil-window-right)
  (evil-leader/set-key "r" nil)
  (evil-leader/set-key "rl" 'ivy-resume)
  (evil-leader/set-key "j" nil)
  (evil-leader/set-key "j=" 'yq/indent-region-or-buffer)
  (evil-mode 1))
;; ;; ( evil-set-initial-state MODE STATE)

(use-package evil-snipe
  :straight t
  :init
  (setq evil-snipe-scope 'whole-buffer
	evil-snipe-enable-highlight t
	evil-snipe-enable-incremental-highlight t
	evil-snipe-auto-disable-substitute t
	evil-snipe-show-prompt nil
	evil-snipe-smart-case t)
  :config
  ;; remap s
  ;; use t as evil-snipe-s in normal mode
  (evil-define-key* '(normal motion) evil-snipe-local-mode-map
		    "s" nil
		    "S" nil
		    "t" #'evil-snipe-s
		    "T" #'evil-snipe-S)
  (setq evil-snipe-auto-disable-substitute nil)
  (evil-snipe-mode 1)
  (setq evil-snipe-repeat-scope 'whole-buffer)
  (evil-snipe-override-mode 1))

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1))

(use-package evil-args
  :straight t
  :config ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

(use-package evil-search-highlight-persist
  :straight t
  :config
  (global-evil-search-highlight-persist t)
  (setq evil-search-highlight-string-min-len 1)
  evil-search-highlight-persist-all-windows t)

(use-package evil-textobj-anyblock
  :straight t
  :config
  (define-key evil-inner-text-objects-map "f" 'evil-textobj-anyblock-inner-block)
  (define-key evil-outer-text-objects-map "f" 'evil-textobj-anyblock-a-block))
