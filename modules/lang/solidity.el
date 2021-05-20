(use-package solidity-mode
  :straight t
  :mode "\\.sol\\'"
  :custom
  (solidity-comment-style 'star))

(use-feature solidity-flycheck
  :load-path "straight/build/solidity-mode"
  :custom
  (solidity-flycheck-solc-checker-active t)
  :defer t
  :init
  (add-hook 'solidity-mode-hook (defl (require 'solidity-flycheck))))

(use-feature company-solidity
  :load-path "straight/build/solidity-mode"
  :defer t
  :init
  (spacemacs|add-company-backends
    :backends (company-solidity)
    :modes solidity-mode
    :variables
    company-minimum-prefix-length 0
    :after-hook t))