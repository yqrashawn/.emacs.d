(use-package help-fns-plus
  :straight t
  :commands (describe-keymap)
  :init
  (spacemacs/set-leader-keys "hdK" 'describe-keymap))
