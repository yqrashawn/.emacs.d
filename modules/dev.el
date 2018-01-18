(use-package help-fns+
  :straight t
  :commands (describe-keymap)
  :init
  (evil-leader/set-key "hdK" 'describe-keymap))