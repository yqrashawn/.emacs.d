(use-package parinfer
  :straight t
  :defer t
  ;; :hook (emacs-lisp-mode . parinfer-mode)
  :init
  (setq parinfer-lighters '("Par:I" . "Par:P"))
  (setq parinfer-extensions '(defaults pretty-parens evil smart-yank))
  :config
  (define-key parinfer-mode-map (kbd "C-,") 'parinfer-toggle-mode))