(yq/add-toggle parinfer :mode parinfer-mode)

(defun yq/toggle-parinfer-mode ()
  (interactive)
  (if (bound-and-true-p parinfer-mode)
      (parinfer-mode -1)
    (parinfer-mode 1)))

(defun crux-start-or-switch-to (function buffer-name)
  "Invoke FUNCTION if there is no buffer with BUFFER-NAME.
Otherwise switch to the buffer named BUFFER-NAME.  Don't clobber
the current buffer."
  (if (not (get-buffer buffer-name))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (funcall function))
    (switch-to-buffer-other-window buffer-name)))

(use-package ielm
  :straight t
  :init(add-hook 'ielm-mode-hook #'rainbow-delimiters-mode)
  :config (define-key inferior-emacs-lisp-mode-map (kbd "C-c C-z") 'kill-buffer-and-window))

(use-package elisp-mode
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :diminish (emacs-lisp-mode . "EL")
  :commands (emacs-lisp-mode)
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq-local evil-shift-width 1)))
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "λ")))
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (defun bozhidar-visit-ielm ()
    "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
    (interactive)
    (crux-start-or-switch-to 'ielm "*ielm*"))
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'bozhidar-visit-ielm)
  (spacemacs|define-jump-handlers emacs-lisp-mode)
  (spacemacs|define-jump-handlers lisp-interaction-mode)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)
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

(use-package lispy
  :straight t
  :diminish lispy " ʪ"
  :init
  (customize-set-variable 'lispy-eval-display-style 'overlay)
  (customize-set-variable 'lispy-visit-method 'projectile)
  (customize-set-variable 'lispy-safe-copy t)
  (customize-set-variable 'lispy-safe-delete t)
  (yq/add-toggle lispy :mode lispy-mode)
  (spacemacs/set-leader-keys "," 'yq/toggle-lispy)
  :config
  (advice-add #'special-lispy-eval :before (lambda () (or (fboundp 'cider--make-overlay) (require 'cider))))
  ;; (evil-define-key 'insert lispy-mode-map-special "o" 'evil-execute-in-normal-state)
  (evil-define-key 'insert lispy-mode-map (kbd "C-k") 'lispy-kill)
  (evil-define-key 'insert lispy-mode-map (kbd "C-d") 'lispy-delete)
  (evil-define-key 'insert lispy-mode-map (kbd "C-r") 'undo-tree-redo)
  (evil-define-key 'insert lispy-mode-map (kbd "C-e") 'lispy-move-end-of-line)
  (evil-define-key 'normal lispy-mode-map "sl" 'lispy-goto)
  ;; (evil-define-key 'normal lispy-mode-map "b" 'sp-previous-sexp)
  ;; (evil-define-key 'normal lispy-mode-map "e" 'sp-next-sexp)
  (push '("*lispy-message*" :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
  (define-key evil-normal-state-map (kbd "C-a") 'mwim-beginning-of-code-or-line))

(use-package parinfer
  :straight (:host github :repo "yqrashawn/parinfer-mode")
  :after lispy
  :bind
  (("C-," . parinfer-toggle-mode))
  :hook ((clojure-mode .  parinfer-mode)
         (emacs-lisp-mode . parinfer-mode)
         (lisp-mode . parinfer-mode))
  :commands (parinfer-mode parinfer-mode-enable parinfer-toggle-mode)
  :init
  (setq parinfer-auto-switch-indent-mode t)
  (setq parinfer-auto-switch-indent-mode-when-closing t)
  (setq parinfer-lighters '(" Par:I" . " Par:P"))
  (setq parinfer-extensions
        '(defaults       ; should be included.
           pretty-parens  ; different paren styles for different modes.
           evil           ; If you use Evil.
           lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
           smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
           smart-yank)))

(use-package eval-sexp-fu
  :straight t
  :commands (eval-sexp-fu-flash-mode)
  :hook (emacs-lisp-mode . eval-sexp-fu-flash-mode))