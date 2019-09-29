(use-package help-fns+
  :straight (:host github :repo "emacsmirror/help-fns-plus")
  :commands (describe-keymap)
  :init
  (spacemacs/set-leader-keys "hdK" 'describe-keymap))
