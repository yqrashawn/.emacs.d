 (use-package iedit :straight t)

(use-package expand-region
  :straight t
  :config
  (define-key evil-normal-state-map "sv" 'er/expand-region)
  (setq expand-region-contract-fast-key "V")
  expand-region-reset-fast-key "r")

(use-package evil-iedit-state :straight t)