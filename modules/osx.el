(use-package reveal-in-osx-finder
  :straight t
  :if (spacemacs/system-is-mac)
  :commands reveal-in-osx-finder
  :init
  (evil-leader/set-key "bf" 'reveal-in-osx-finder))

(use-package osx-trash
  :straight t
  :if (and (spacemacs/system-is-mac)
	   (not (boundp 'mac-system-move-file-to-trash-use-finder)))
  :init (osx-trash-setup))

(use-package pbcopy
  :straight t
  :if (and (spacemacs/system-is-mac) (not (display-graphic-p)))
  :init (turn-on-pbcopy))
