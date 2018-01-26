(use-package git-commit
  :straight t)
(use-package magit-popup
  :straight t)

(use-package magit
  :straight t
  :config
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-mode)
  (add-hook 'git-rebase-mode-hook 'turn-off-evil-snipe-mode)
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
  (add-hook 'git-rebase-mode-hook 'turn-off-evil-snipe-override-mode)
  (spacemacs/set-leader-keys "g" nil)
  (spacemacs/set-leader-keys "gf" 'magit-file-popup)
  (spacemacs/set-leader-keys "gs" 'magit-status))

(use-package evil-magit :straight t)

(use-package with-editor
  :straight t
  :commands (with-editor-mode)
  :hook (git-commit-mode-hook . with-editor-hook)
  :init
  (spacemacs/set-leader-keys "hdK" 'describe-keymap))

(use-package magithub
  :straight t
  :after magit
  :config
  (setq magithub-enabled-by-default nil)
  (setq magithub-features t
        magithub-feature-autoinject t
        magithub-dir "~/Dropbox/sync/magithub"))