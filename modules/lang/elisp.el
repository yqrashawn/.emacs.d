(use-package elisp-mode
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :commands (emacs-lisp-mode)
  :config
  (evil-define-key 'normal 'emacs-lisp-mode-map "," nil)
  (evil-define-key 'normal 'emacs-lisp-mode-map ",cc" 'emacs-lisp-byte-compile)
  (evil-define-key 'normal 'emacs-lisp-mode-map ",cc" 'emacs-lisp-byte-compile)
  (evil-define-key 'normal 'emacs-lisp-mode-map ",eb" 'eval-buffer)
  (evil-define-key 'normal 'emacs-lisp-mode-map ",ee" 'eval-last-sexp)
  (evil-define-key 'normal 'emacs-lisp-mode-map ",ee" 'eval-defun)
  (evil-define-key 'normal 'emacs-lisp-mode-map ",el" 'lisp-state-eval-sexp-end-of-line)
  (evil-define-key 'normal 'emacs-lisp-mode-map ",er" 'eval-reason-sexp))

(use-package parinfer
  :straight t
  ;; :hook (emacs-lisp-mode . parinfer-mode)
  :commands (parinfer-mode parinfer-mode-enable)
  :init
  (defun yq/toggle-parinfer-mode ()
    (if (boundp 'parinfer-mode)
        (parinfer-mode-disable)
      (parinfer-mode-enable)))
  (setq parinfer-lighters '("Par:I" . "Par:P"))
  (setq parinfer-extensions '(defaults pretty-parens evil smart-yank))
  (evil-define-key 'normal 'emacs-lisp-mode-map ",m" 'yq/toggle-parinfer-mode)
  :config
  (define-key parinfer-mode-map (kbd "C-,") 'parinfer-toggle-mode))

(use-package eldoc
  :defer t
  :commands (eldoc-mode)
  :hook (emacs-lisp-mode . eldoc-mode)
  :config
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))
