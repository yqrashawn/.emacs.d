(use-package vue-mode
  :straight t
  ;; :mode ("\\.vue\\'" . vue-mode)
  :hook (vue-mode . hs-minor-mode)
  :hook (vue-mode . prettier-js-mode)
  :config
  (setq mmm-submode-decoration-level 2)
  (spacemacs/enable-flycheck 'vue-mode))

;; (use-package lsp-mode
;;   :mode ("\\.vue\\'" . lsp-mode)
;;   :straight t)

;; (use-package lsp-vue
;;   :straight t
;;   :mode ("\\.vue\\'" . lsp-vue-mm-enable)
;;   :after lsp-mode)

;; (use-package company-lsp
;;   :straight t
;;   :after lsp-mode
;;   :config
;;   (spacemacs|add-company-backends
;;     :backends (company-tabnine company-lsp)
;;     :modes web-mode
;;     :after-hook t))

;; (use-package lsp-ui
;;   :straight t
;;   :hook (lsp-mode . lsp-ui-mode))