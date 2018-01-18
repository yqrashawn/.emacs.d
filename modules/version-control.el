(use-package git-commit
  :straight t)
(use-package magit-popup
  :straight t)

(use-package magit
  :straight t
  :config
  (evil-leader/set-key "g" nil)
  (evil-leader/set-key "gs" 'magit-status))

(use-package evil-magit :straight t)

(use-package with-editor
  :straight t
  :commands (with-editor-mode)
  :hook (git-commit-mode-hook . with-editor-hook)
  :init
  (evil-leader/set-key "hdK" 'describe-keymap))