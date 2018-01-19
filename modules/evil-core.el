(yq/get-modules "evil-core-funcs.el")
(global-set-key (kbd "C-g") 'keyboard-quit)

(use-package undo-tree
  :straight (:host github :repo "emacsmirror/undo-tree")
  :diminish undo-tree-mode
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
  (customize-set-variable 'evil-move-cursor-back nil)
  (customize-set-variable 'evil-want-C-u-scroll t)
  (customize-set-variable 'evil-want-Y-yank-to-eol t)
  (customize-set-variable 'evil-symbol-word-search t)
  (customize-set-variable 'evil-kill-on-visual-paste nil)
  (customize-set-variable 'evil-esc-delay 0)
  (customize-set-variable 'evil-shift-width 2)
  (customize-set-variable 'evil-show-paren-range 1)
  :config
  (define-key evil-normal-state-map "gn" 'evil-search-word-forward)
  (define-key evil-normal-state-map "zl" 'hs-hide-level)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-toggle-fold)
  (define-key evil-normal-state-map "s" nil)
  (define-key evil-normal-state-map "sk" 'yq/kill-this-buffer)
  (define-key evil-normal-state-map "sc" 'yq/delete-window)
  (define-key evil-normal-state-map "sh" 'save-buffer)
  (define-key evil-normal-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
  (define-key evil-normal-state-map (kbd "C-m") 'evil-jump-item)
  (define-key evil-visual-state-map (kbd "C-m") 'evil-jump-item)
  (define-key evil-visual-state-map (kbd "C-e") 'evil-end-of-line)
  (define-key evil-visual-state-map (kbd "C-a") 'evil-first-non-blank)
  (define-key evil-insert-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
  (define-key evil-insert-state-map (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (define-key evil-insert-state-map (kbd "C-n") 'next-line)
  (define-key evil-insert-state-map (kbd "C-p") 'previous-line)
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
  :diminish evil-snipe-mode
  :diminish evil-snipe-override-mode
  :diminish evil-snipe-local-mode
  :init
  (setq evil-snipe-scope 'whole-buffer)
  (setq evil-snipe-enable-highlight t)
  (setq evil-snipe-enable-incremental-highlight t)
  (setq evil-snipe-auto-disable-substitute t)
  (setq evil-snipe-show-prompt nil)
  (setq evil-snipe-smart-case t)
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
  (global-evil-surround-mode 1)
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute))

(use-package evil-args
  :straight t
  :config ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

(use-package evil-search-highlight-persist
  :straight t
  :diminish global-highlight-parentheses-mode
  :config
  (set-face-attribute 'evil-search-highlight-persist-highlight-face nil
		      :inherit 'lazy-highlight
		      :background nil
		      :foreground nil)
  (global-evil-search-highlight-persist t)
  (setq evil-search-highlight-string-min-len 1)
  evil-search-highlight-persist-all-windows t)

(use-package evil-textobj-anyblock
  :straight t
  :config
  (define-key evil-inner-text-objects-map "f" 'evil-textobj-anyblock-inner-block)
  (define-key evil-outer-text-objects-map "f" 'evil-textobj-anyblock-a-block))

(use-package evil-mc
  :straight t
  :diminish evil-mc-mode
  :init
  (setq evil-mc-one-cursor-show-mode-line-text nil)
  (global-evil-mc-mode t)
  :init (add-hook 'after-init-hook #'global-evil-mc-mode)
  :config
  ;; this is ugly
  (evil-define-key 'normal evil-mc-key-map (kbd "C-g") 'evil-mc-undo-all-cursors)
  (evil-define-key 'visual evil-mc-key-map (kbd "C-g") 'keyboard-quit)
  (setq evil-mc-mode-line-text-cursor-color t)
  (define-key evil-normal-state-map (kbd "C-n") 'evil-mc-make-and-goto-next-match)
  (define-key evil-normal-state-map (kbd "C-p") 'evil-mc-make-and-goto-prev-match)
  (define-key evil-normal-state-map (kbd "C-S-n") 'evil-mc-skip-and-goto-next-match)
  (define-key evil-normal-state-map (kbd "M-j") 'evil-mc-make-cursor-move-next-line)
  (define-key evil-normal-state-map (kbd "M-k") 'evil-mc-make-cursor-move-prev-line)
  (define-key evil-normal-state-map (kbd "<C-return>") 'evil-mc-make-all-cursors))

(use-package evil-matchit
  :straight t
  :init (add-hook 'after-init-hook #'global-evil-matchit-mode))

(use-package anzu
  :straight t
  :diminish anzu-mode
  :init
  (global-anzu-mode t)
  :config
  (defun yq/anzu-update-func (here total)
    (when anzu--state
      (let ((status (cl-case anzu--state
		      (search (format "<%d/%d>" here total))
		      (replace-query (format "(%d Replaces)" total))
		      (replace (format "<%d/%d>" here total)))))
	(propertize status 'face 'anzu-mode-line))))

  (custom-set-variables
   '(anzu-mode-line-update-function #'yq/anzu-update-func))
  (setq anzu-cons-mode-line-p nil)
  (setcar (cdr (assq 'isearch-mode minor-mode-alist))
	  '(:eval (anzu--update-mode-line))))

(use-package evil-anzu
  :straight t
  :diminish anzu-mode
  :init
  (global-anzu-mode t)
  :config
  (setq anzu-search-threshold 1000)
  (setq anzu-cons-mode-line-p nil))
