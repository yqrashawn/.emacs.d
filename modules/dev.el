(use-package help-fns+
  :straight t
  :commands (describe-keymap)
  :init
  (spacemacs/set-leader-keys "hdK" 'describe-keymap))
