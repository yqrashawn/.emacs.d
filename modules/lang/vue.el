(use-package vue-mode
  :straight t
  :mode ("\\.vue\\'" . vue-mode))

(use-package lsp-mode
  :straight t
  :after vue-mode
  :config (require 'lsp-flycheck))

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