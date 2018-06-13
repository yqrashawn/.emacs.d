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
  (evil-define-key 'normal emacs-lisp-mode-map "," nil)
  (evil-define-key 'normal emacs-lisp-mode-map ",m" 'yq/toggle-parinfer)
  (evil-define-key 'normal emacs-lisp-mode-map ",cc" 'emacs-lisp-byte-compile)
  (evil-define-key 'normal emacs-lisp-mode-map ",cc" 'emacs-lisp-byte-compile)
  (evil-define-key 'normal emacs-lisp-mode-map ",eb" 'eval-buffer)
  (evil-define-key 'normal emacs-lisp-mode-map ",ee" 'eval-last-sexp)
  (evil-define-key 'normal emacs-lisp-mode-map ",ef" 'eval-defun)
  (evil-define-key 'normal emacs-lisp-mode-map ",el" 'lisp-state-eval-sexp-end-of-line)
  (evil-define-key 'visual emacs-lisp-mode-map ",er" 'eval-region))

(use-package elisp-slime-nav
  :straight t
  :diminish elisp-slime-nav-mode
  :defer t
  :hook (emacs-lisp-mode . elisp-slime-nav-mode)
  :init
  (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
    (evil-define-key 'normal emacs-lisp-mode-map ",hh" 'elisp-slime-nav-describe-elisp-thing-at-point)
    (let ((jumpl (intern (format "spacemacs-jump-handlers-%S" mode))))
      (add-to-list jumpl 'elisp-slime-nav-find-elisp-thing-at-point))))

;; (use-package parinfer
;;   :straight t
;;   ;; :hook (emacs-lisp-mode . parinfer-mode)
;;   :commands (parinfer-mode parinfer-mode-enable parinfer-toggle-mode)
;;   :init
;;   (setq parinfer-lighters '(" Par:I" . " Par:P"))
;;   (setq parinfer-extensions '(defaults pretty-parens evil smart-yank))
;;   :config
;;   (define-key parinfer-mode-map (kbd "C-,") 'parinfer-toggle-mode))

(use-package lispy
  :straight t
  :diminish lispy " ʪ"
  :commands (lispy-mode)
  :hook (emacs-lisp-mode . lispy-mode)
  :init
  (customize-set-variable 'lispy-visit-method 'projectile)
  (yq/add-toggle lispy :mode lispy-mode)
  (define-key evil-normal-state-map ",," 'yq/toggle-lispy)
  :config
  (evil-define-key 'insert lispy-mode-map (kbd "C-k") 'lispy-kill)
  (evil-define-key 'insert lispy-mode-map (kbd "C-r") 'undo-tree-redo)
  (evil-define-key 'insert lispy-mode-map (kbd "C-e") 'lispy-move-end-of-line)
  (evil-define-key 'normal lispy-mode-map "sl" 'lispy-goto)
  (evil-define-key 'normal lispy-mode-map "b" 'sp-previous-sexp)
  (evil-define-key 'normal lispy-mode-map "e" 'sp-next-sexp)
  (push '("*lispy-message*" :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config))

(use-package hl-sexp
  :straight (:host github :repo "emacsattic/hl-sexp")
  :commands (hl-sexp-mode)
  :init
  (yq/add-toggle hl-sexp :mode hl-sexp-mode)
  (evil-define-key 'normal emacs-lisp-mode-map ",th" 'yq/toggle-hl-sexp))

;; (use-package eval-sexp-fu
;;   :straight t)

;; :config
;; (eval-after-load 'hl-sexp
;; (defadvice hl-sexp-mode (after unflicker (turn-on) activate)
;;   (when turn-on
;;     (remove-hook 'pre-command-hook #'hl-sexp-unhighlight)))
;; ))
