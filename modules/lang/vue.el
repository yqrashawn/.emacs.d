(use-package vue-mode
  :straight t
  ;; :mode ("\\.vue\\'" . vue-mode)
  :hook (vue-mode . hs-minor-mode)
  :hook (vue-mode . prettier-js-mode)
  :config
  (setq mmm-submode-decoration-level 2)
  (spacemacs/enable-flycheck 'vue-mode))
