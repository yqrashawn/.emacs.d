(setq-default mac-option-modifier 'meta)
(setq-default mac-command-modifier 'super)
(global-set-key (kbd "s-w") #'delete-window)
(global-set-key (kbd "s-q") #'save-buffers-kill-emacs)
(global-set-key (kbd "s-v") #'yank)
(global-set-key (kbd "s-c") #'kill-ring-save)
(global-set-key (kbd "s-z") #'undo-tree-undo)
(global-set-key (kbd "s-s") #'save-buffer)

(and (boundp 'ns-do-hide-emacs) (global-set-key (kbd "s-h" #'ns-do-hide-emacs)))

(use-package reveal-in-osx-finder
  :straight t
  :if (spacemacs/system-is-mac)
  :commands reveal-in-osx-finder
  :init
  (spacemacs/set-leader-keys "bf" 'reveal-in-osx-finder))

(use-package osx-trash
  :straight t
  :if (and (spacemacs/system-is-mac)
	   (not (boundp 'mac-system-move-file-to-trash-use-finder)))
  :init (osx-trash-setup))

(use-package pbcopy
  :straight t
  :disabled t
  :if (and (spacemacs/system-is-mac) (in-terminal-p))
  :init (turn-on-pbcopy))

(use-package edit-server
  :straight t
  :config (edit-server-start))