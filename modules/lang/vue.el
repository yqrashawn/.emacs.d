(use-package vue-mode
  :straight t
  :mode ("\\.vue\\'" . vue-mode)
  :hook (vue-mode . hs-minor-mode)
  :hook (vue-mode . prettier-js-mode)
  :config
  (setq mmm-submode-decoration-level 2)
  (spacemacs/enable-flycheck 'vue-mode))

(use-package lsp-mode
  :straight t
  :after vue-mode)

(use-package lsp-vue
  :straight t
  :after lsp-mode
  :hook (vue-mode-hook . lsp-vue-mm-enable))

(use-package company-lsp
  :straight t
  :after lsp-mode
  :config
  (spacemacs|add-company-backends
    :backends company-lsp
    :modes vue-mode))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode))