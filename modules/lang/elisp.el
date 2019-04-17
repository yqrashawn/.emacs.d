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
  :init
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode)
  (spacemacs|define-jump-handlers ielm-mode)
  :config (define-key inferior-emacs-lisp-mode-map (kbd "C-c C-z") 'kill-buffer-and-window))

(use-package elisp-mode
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :diminish (emacs-lisp-mode . "EL")
  :commands (emacs-lisp-mode)
  :init
  (spacemacs|define-jump-handlers emacs-lisp-mode)
  (spacemacs|define-jump-handlers lisp-interaction-mode)
  ;; (add-hook 'emacs-lisp-mode-hook
  ;;           (lambda () (setq-local lisp-indent-function #'common-lisp-indent-function)))
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (setq-local evil-shift-width 1)
                                    (setq-local company-idle-delay 0.2)
                                    (setq mode-name "λ")
                                    (setq-local company-backends '(company-capf
                                                                   company-tabnine
                                                                   (company-dabbrev-code
                                                                    company-gtags
                                                                    company-etags
                                                                    company-keywords)
                                                                   company-files
                                                                   company-dabbrev))))
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  ;; Idea from http://www.reddit.com/r/emacs/comments/312ge1/i_created_this_function_because_i_was_tired_of/
  (defun spacemacs/eval-current-form ()
    "Find and evaluate the current def* or set* command.
Unlike `eval-defun', this does not go to topmost function."
    (interactive)
    (save-excursion
      (search-backward-regexp "(def\\|(set")
      (forward-list)
      (call-interactively 'eval-last-sexp)))

  (defun spacemacs/nav-find-elisp-thing-at-point-other-window ()
    "Find thing under point and go to it another window."
    (interactive)
    (let ((symb (variable-at-point)))
      (if (and symb
               (not (equal symb 0))
               (not (fboundp symb)))
          (find-variable-other-window symb)
        (find-function-at-point))))

  ;; smartparens integration
  (defun spacemacs/eval-current-form-sp (&optional arg)
    "Call `eval-last-sexp' after moving out of one level of
parentheses. Will exit any strings and/or comments first.
An optional ARG can be used which is passed to `sp-up-sexp' to move out of more
than one sexp.
Requires smartparens because all movement is done using `sp-up-sexp'."
    (interactive "p")
    (require 'smartparens)
    (let ((evil-move-beyond-eol t))
      ;; evil-move-beyond-eol disables the evil advices around eval-last-sexp
      (save-excursion
        (let ((max 10))
          (while (and (> max 0)
                      (sp-point-in-string-or-comment))
            (decf max)
            (sp-up-sexp)))
        (sp-up-sexp arg)
        (call-interactively 'eval-last-sexp))))

  (defun spacemacs/eval-current-symbol-sp ()
    "Call `eval-last-sexp' on the symbol around point.
Requires smartparens because all movement is done using `sp-forward-symbol'."
    (interactive)
    (require 'smartparens)
    (let ((evil-move-beyond-eol t))
      ;; evil-move-beyond-eol disables the evil advices around eval-last-sexp
      (save-excursion
        (sp-forward-symbol)
        (call-interactively 'eval-last-sexp))))

  (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'rtog/toggle-repl)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)
  (evil-define-key 'normal emacs-lisp-mode-map "," nil)
  (evil-define-key 'normal emacs-lisp-mode-map ",ec" 'spacemacs/eval-current-form-sp)
  (evil-define-key 'normal emacs-lisp-mode-map ",es" 'spacemacs/eval-current-symbol-sp)
  (evil-define-key 'normal emacs-lisp-mode-map ",eC" 'spacemacs/eval-current-form)
  (evil-define-key 'normal emacs-lisp-mode-map ",gg" 'spacemacs/nav-find-elisp-thing-at-point-other-window)
  (evil-define-key 'normal emacs-lisp-mode-map ",m" 'yq/toggle-parinfer)
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
  (dolist (mode '(emacs-lisp-mode lisp-interaction-mode ielm-mode))
    (evil-define-key 'normal emacs-lisp-mode-map ",hh" 'elisp-slime-nav-describe-elisp-thing-at-point)
    (let ((jumpl (intern (format "spacemacs-jump-handlers-%S" mode))))
      (add-to-list jumpl 'elisp-slime-nav-find-elisp-thing-at-point))))

(use-package elisp-def
  :straight t
  :hook ((emacs-lisp-mode ielm-mode) . elisp-def-mode)
  :after elisp-mode
  :diminish elisp-def-mode
  :init
  (dolist (mode '(emacs-lisp-mode lisp-interaction-mode ielm-mode))
    (let ((jumpl (intern (format "spacemacs-jump-handlers-%S" mode))))
      (add-to-list jumpl 'elisp-def))))

(use-package lispy
  :straight t
  :diminish lispy " ʪ"
  :custom
  (lispy-eval-display-style 'overlay)
  (lispy-visit-method 'projectile)
  (lispy-safe-copy t)
  (lispy-safe-delete t)
  (lispy-safe-paste t)
  (lispy-safe-actions-no-pull-delimiters-into-comments t)
  ;; :hook ((lisp-mode emacs-lisp-mode ielm-mode clojure-mode clojurescript-mode) . lispy-mode)
  :init
  (yq/add-toggle lispy :mode lispy-mode)
  :config
  (advice-add
   #'special-lispy-eval
   :before (lambda ()
             (or (fboundp 'cider--make-overlay)
                 (require 'cider))))
  (defhydra lh-knight ()
    "knight"
    ("j" lispy-knight-down)
    ("k" lispy-knight-up)
    ("r" evil-open-folds :exit t)
    ("h" save-buffer :exit t)
    ("l" (lambda (arg)
           (interactive "p")
           (backward-char)
           (hs-hide-level arg)
           (forward-char)) :exit t)
    ("z" nil))
  (define-key lispy-mode-map (kbd "C-x C-6 q") #'lispy-describe-inline)
  (define-key lispy-mode-map (kbd "C-x C-6 w") #'lispy-arglist-inline)
  ;; (evil-define-key 'insert lispy-mode-map (kbd "C-k") 'lispy-kill)
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
  (defun +parinfer-hs-toggle-folding ()
    (interactive)
    (if company-my-keymap
        (company-select-previous)
      (progn
        (hs-toggle-hiding)
        (backward-char))))
  (setq parinfer-lighters '(" Par:I" . " Par:P"))
  (setq parinfer-display-error t)
  (setq parinfer-indent-mode-confirm nil)
  (setq parinfer-extensions
        '(defaults       ; should be included.
           pretty-parens  ; different paren styles for different modes.
           evil           ; If you use Evil.
           lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
           lispyville
           smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
           smart-yank))
  :config
  (evil-define-key 'insert parinfer-mode-map (kbd "C-k") '+parinfer-hs-toggle-folding)
  (define-key parinfer-mode-map (kbd "C-k") '+parinfer-hs-toggle-folding)
  (define-key parinfer-mode-map (kbd "C-x C-6 q") #'lispy-describe-inline)
  (define-key parinfer-mode-map (kbd "C-x C-6 w") #'lispy-arglist-inline)
  ;; (define-key parinfer-mode-map (kbd "C-k") #'lispy-kill)
  (define-key parinfer-mode-map (kbd "C-d") #'lispy-delete)
  (define-key parinfer-mode-map (kbd "C-d") #'lispy-delete)
  (define-key parinfer-mode-map (kbd "C-a") #'lispy-move-beginning-of-line)
  (define-key parinfer-mode-map (kbd "C-e") #'lispy-move-end-of-line)
  ;; (evil-define-key 'normal parinfer-mode-map (kbd "C-a") #'lispy-move-beginning-of-line)
  (evil-define-key 'normal parinfer-mode-map (kbd "C-e") #'lispy-move-end-of-line)
  ;; (evil-define-key 'insert parinfer-mode-map (kbd "C-a") #'lispy-move-beginning-of-line)
  (evil-define-key 'insert parinfer-mode-map (kbd "C-e") #'lispy-move-end-of-line)
  (evil-define-key 'normal parinfer-mode-map "si" #'lispy-mark-symbol))

(use-package eval-sexp-fu
  :straight t
  :commands (eval-sexp-fu-flash-mode)
  :hook (emacs-lisp-mode . eval-sexp-fu-flash-mode))

(use-package edebug
  :commands (edebug-defun)
  :init
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)
  (defun spacemacs/edebug-instrument-defun-on ()
    "Toggle on instrumentalisation for the function under `defun'."
    (interactive)
    (eval-defun 'edebugit))

  (defun spacemacs/edebug-instrument-defun-off ()
    "Toggle off instrumentalisation for the function under `defun'."
    (interactive)
    (eval-defun nil))

  (defun spacemacs/elisp-toggle-debug-expr-and-eval-func ()
    "Insert or remove debug expression, evaluate function and save buffer."
    (interactive)
    (let ((trace "(debug)")
          (line (thing-at-point 'line)))
      (if (and line (string-match trace line))
          (kill-whole-line)
        (progn
          (back-to-indentation)
          (insert trace)
          (newline-and-indent))))
    (eval-defun nil)
    (save-buffer))

  (defun spacemacs//edebug-mode (&rest args)
    "Additional processing when `edebug-mode' is activated or deactivated."
    (if (not edebug-mode)
        ;; disable edebug-mode
        (evil-normal-state)
      ;; enable edebug-mode
      (evil-evilified-state)
      (evil-normalize-keymaps)
      (when (and (fboundp 'golden-ratio-mode)
                 golden-ratio-mode)
        (golden-ratio))))

  ;; key bindings
  (evil-define-key 'normal emacs-lisp-mode-map
    ",df" 'spacemacs/edebug-instrument-defun-on
    ",dF" 'spacemacs/edebug-instrument-defun-off)

  (evil-define-key 'normal lisp-interaction-mode-map
    ",df" 'spacemacs/edebug-instrument-defun-on
    ",dF" 'spacemacs/edebug-instrument-defun-off)

  ;; since we evilify `edebug-mode-map' we don't need to intercept it to
  ;; make it work with evil
  (evil-set-custom-state-maps
   'evil-intercept-maps
   'evil-pending-intercept-maps
   'intercept-state
   'evil-make-intercept-map
   (delq (assq 'edebug-mode-map evil-intercept-maps)
         evil-intercept-maps))
  (evilified-state-evilify-map edebug-mode-map
    :eval-after-load edebug
    :bindings
    "a" #'edebug-stop
    "c" #'edebug-go-mode
    "s" #'edebug-step-mode
    "S" #'edebug-next-mode)

  (evilified-state-evilify-map edebug-eval-mode-map
    :eval-after-load edebug
    :bindings
    "a" #'edebug-stop
    "c" #'edebug-go-mode
    "s" #'edebug-step-mode
    "S" #'edebug-next-mode)
  (advice-add 'edebug-mode :after 'spacemacs//edebug-mode))
;;   (defhydra hydra-edebug (:color amaranth
;;                                  :hint  nil)
;;     "
;;     EDEBUG MODE
;; ^^_<SPC>_ step             ^^_f_ forward sexp         _b_reakpoint set                previous _r_esult      _w_here                    ^^_d_ebug backtrace
;; ^^_n_ext                   ^^goto _h_ere              _u_nset breakpoint              _e_val expression      bounce _p_oint             _q_ top level (_Q_ nonstop)
;; _g_o (_G_ nonstop)         ^^_I_nstrument callee      next _B_reakpoint               _E_val list            _v_iew outside             ^^_a_bort recursive edit
;; _t_race (_T_ fast)         step _i_n/_o_ut            _x_ conditional breakpoint      eval _l_ast sexp       toggle save _W_indows      ^^_S_top
;; _c_ontinue (_C_ fast)      ^^^^                       _X_ global breakpoint
;; "
;;     ("<SPC>" edebug-step-mode)
;;     ("n"     edebug-next-mode)
;;     ("g"     edebug-go-mode)
;;     ("G"     edebug-Go-nonstop-mode)
;;     ("t"     edebug-trace-mode)
;;     ("T"     edebug-Trace-fast-mode)
;;     ("c"     edebug-continue-mode)
;;     ("C"     edebug-Continue-fast-mode)

;;     ("f"     edebug-forward-sexp)
;;     ("h"     edebug-goto-here)
;;     ("I"     edebug-instrument-callee)
;;     ("i"     edebug-step-in)
;;     ("o"     edebug-step-out)

;;     ;; breakpoints
;;     ("b"     edebug-set-breakpoint)
;;     ("u"     edebug-unset-breakpoint)
;;     ("B"     edebug-next-breakpoint)
;;     ("x"     edebug-set-conditional-breakpoint)
;;     ("X"     edebug-set-global-break-condition)

;;     ;; evaluation
;;     ("r"     edebug-previous-result)
;;     ("e"     edebug-eval-expression)
;;     ("l"     edebug-eval-last-sexp)
;;     ("E"     edebug-visit-eval-list)

;;     ;; views
;;     ("w"     edebug-where)
;;     ("p"     edebug-bounce-point)
;;     ("v"     edebug-view-outside)       ; maybe obsolete??
;;     ("P"     edebug-view-outside)       ; same as v
;;     ("W"     edebug-toggle-save-windows)

;;     ("d"     edebug-backtrace)

;;     ;; quitting and stopping
;;     ("q"     top-level :color blue)
;;     ("Q"     edebug-top-level-nonstop :color blue)
;;     ("a"     abort-recursive-edit :color blue)
;;     ("S"     edebug-stop :color blue))
;;   (with-eval-after-load 'edebug
;;     (bind-key "?" #'hydra-edebug/body edebug-mode-map)))

;; (use-package elisp-demos
;;   :straight t
;;   :init
;;   (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
;;   (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package lispyville
  :straight (:host github :repo "noctuid/lispyville")
  :after (parinfer lispy)
  :commands (lispyville-mode)
  :hook (parinfer-mode . lispyville-mode)
  :custom
  (lispyville-motions-put-into-special nil)
  (lispyville-key-theme
   '(operators
     c-w
     (escape insert visual)
     prettify
     (additional-movement normal visual motion)
     (atom-movement normal visual motion)
     ;; commentary
     slurp/barf-lispy
     wrap
     additional
     additional-insert
     (additional-wrap normal visual insert)
     mark
     mark-toggle))
  :config
  ;; (advice-add #'lispyville-escape :after (defl (&optional arg) (parinfer--switch-to-indent-mode-1)))
  (lispyville--define-key 'normal "V" #'evil-visual-line)
  (lispyville--define-key 'normal "\C-v" #'evil-visual-block)
  (lispyville--define-key 'normal "{" #'lispyville-previous-opening)
  (lispyville--define-key 'normal "}" #'lispyville-next-opening)
  (lispyville--define-key 'normal "[" #'lispyville-previous-closing)
  (lispyville--define-key 'normal "]" #'lispyville-next-closing)
  (lispy-define-key parinfer-mode-map "v" #'lispyville-toggle-mark-type))
