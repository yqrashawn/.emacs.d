(use-package yaml-mode
  :straight t
  :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
         ("Procfile\\'" . yaml-mode))
  :init (add-hook 'yaml-mode-hook 'yq/toggle-aggressive-indent-on)
  :config
  (spacemacs|add-company-backends :modes yaml-mode)
  (spacemacs/enable-flycheck 'yaml-mode)
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))