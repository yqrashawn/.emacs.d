(yq/add-toggle parinfer :mode parinfer-mode)
(defun yq/toggle-parinfer-mode ()
  (interactive)
  (if (bound-and-true-p parinfer-mode)
      (parinfer-mode -1)
    (parinfer-mode 1)))

(use-package elisp-mode
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :commands (emacs-lisp-mode)
  :config
  (spacemacs|define-jump-handlers emacs-lisp-mode)
  (spacemacs|define-jump-handlers lisp-interaction-mode)
  (evil-define-key 'normal 'emacs-lisp-mode-map "," nil)
  (evil-define-key 'normal 'emacs-lisp-mode-map ",m" 'yq/toggle-parinfer)
  (evil-define-key 'normal 'emacs-lisp-mode-map ",cc" 'emacs-lisp-byte-compile)
  (evil-define-key 'normal 'emacs-lisp-mode-map ",cc" 'emacs-lisp-byte-compile)
  (evil-define-key 'normal 'emacs-lisp-mode-map ",eb" 'eval-buffer)
  (evil-define-key 'normal 'emacs-lisp-mode-map ",ee" 'eval-last-sexp)
  (evil-define-key 'normal 'emacs-lisp-mode-map ",ef" 'eval-defun)
  (evil-define-key 'normal 'emacs-lisp-mode-map ",el" 'lisp-state-eval-sexp-end-of-line)
  (evil-define-key 'visual 'emacs-lisp-mode-map ",er" 'eval-region))

(use-package elisp-slime-nav
  :straight t
  :diminish elisp-slime-nav-mode
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
  (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
    (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "mhh" 'elisp-slime-nav-describe-elisp-thing-at-point)
    (let ((jumpl (intern (format "spacemacs-jump-handlers-%S" mode))))
      (add-to-list jumpl 'elisp-slime-nav-find-elisp-thing-at-point))))

(use-package parinfer
  :straight t
  ;; :hook (emacs-lisp-mode . parinfer-mode)
  :commands (parinfer-mode parinfer-mode-enable parinfer-toggle-mode)
  :init
  (setq parinfer-lighters '(" Par:I" . " Par:P"))
  (setq parinfer-extensions '(defaults pretty-parens evil smart-yank))
  :config
  (define-key parinfer-mode-map (kbd "C-,") 'parinfer-toggle-mode))

(use-package eldoc
  :diminish eldoc-mode
  :commands (eldoc-mode)
  :hook (emacs-lisp-mode . eldoc-mode)
  :config
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))
