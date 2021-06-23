;;; elisp.el --- configs about elisp -*- lexical-binding: t; -*-

(yq/add-toggle parinfer :mode parinfer-mode)

(defun yq/lispy-file-p ()
  (memq
   major-mode
   '(emacs-lisp-mode
     clojure-mode
     lisp-mode
     scheme-mode)))

(defun yq/toggle-parinfer-mode ()
  (interactive)
  (if (bound-and-true-p parinfer-mode)
      (parinfer-mode -1)
    (parinfer-mode 1)))

(defun crux-start-or-switch-to #'buffer-name
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
  (yq/setup-sp-keys-for-lispy-modes emacs-lisp-mode-map)
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (setq-local evil-shift-width 1)
                                    (setq-local company-idle-delay 0.2)
                                    (setq mode-name "λ")
                                    (setq-local company-backends '(company-capf
                                                                   (company-dabbrev-code
                                                                    company-gtags
                                                                    company-etags
                                                                    company-keywords)
                                                                   company-files
                                                                   company-dabbrev))))
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
  :straight (:host github :repo "abo-abo/lispy"
                   :files (:defaults "lispy-clojure.clj"
                                     "lispy-clojure.cljs"
                                     "lispy-python.py"
                                     "lispy-clojure-test.clj"))
  :diminish lispy " ʪ"
  :hook ((ielm-mode
          lisp-mode
          clojure-mode
          clojurec-mode
          scheme-mode
          racket-mode
          hy-mode
          lfe-mode
          dune-mode
          fennel-mode
          emacs-lisp-mode
          cider-repl-mode
          clojurescript-mode) . lispy-mode)
  :custom
  (lispy-close-quotes-at-end-p t)
  (lispy-eval-display-style 'overlay)
  (lispy-visit-method 'projectile)
  (lispy-safe-copy t)
  (lispy-safe-delete t)
  (lispy-safe-paste t)
  (lispy-safe-actions-no-pull-delimiters-into-comments t)
  (lispy-outline "^;;\\(;+\\|[^#]\\|\\*+\\)")
  :init
  (defun +clj-file-p ()
    (or cider-mode (memq major-mode '(clojure-mode clojurescript-mode clojurec-mode))))
  (add-hook! 'eval-expression-minibuffer-setup-hook
    (defun doom-init-lispy-in-eval-expression-h ()
      "Enable `lispy-mode' in the minibuffer for `eval-expression'."
      (lispy-mode)
      ;; When `lispy-key-theme' has `parinfer', the TAB key doesn't do
      ;; completion, neither (kbd "<tab>"/"TAB"/"C-i")/[tab]/"\C-i" works in
      ;; terminal as tested so remapping is used as a workaround
      (local-set-key (vector 'remap (lookup-key lispy-mode-map (kbd "TAB"))) #'completion-at-point)))
  (yq/add-toggle lispy :mode lispy-mode)
  :config
  (defadvice! +lispy-right (a) :after #'lispy-right (call-interactively #'lispy-tab))
  (defadvice! +lispy-left (a) :after #'lispy-left (call-interactively #'lispy-tab))
  (defadvice! +lispy-up (a) :after #'lispy-up (call-interactively #'lispy-tab))
  (defadvice! +lispy-down (a) :after #'lispy-down (call-interactively #'lispy-tab))
  (defadvice! +lispy-tab (orig-fn)
    :around #'lispy-tab
    (if (or cider-mode (memq major-mode '(clojure-mode clojurescript-mode clojurec-mode)))
        (and (functionp #'clojure-align) (call-interactively #'clojure-align))
      (call-interactively orig-fn)))
  (add-hook 'lispy-mode-hook #'turn-off-smartparens-mode)
  (with-eval-after-load 'semantic
    (defvar-mode-local emacs-lisp-mode semanticdb-find-default-throttle
      '(project omniscience)
      "Search project files, then search this omniscience database.
It is not necessary to do system or recursive searching because of
the omniscience database.")
    (setq-mode-local emacs-lisp-mode
                     semanticdb-find-default-throttle
                     (default-value 'semanticdb-find-default-throttle))

    ;; https://github.com/syl20bnr/spacemacs/issues/7038#issuecomment-255014767
    ;; (eval-after-load 'semantic
    ;;   (add-hook 'semantic-mode-hook
    ;;             (lambda ()
    ;;               (dolist (x (default-value 'completion-at-point-functions))
    ;;                 (when (string-prefix-p "semantic-" (symbol-name x))
    ;;                   (remove-hook 'completion-at-point-functions x))))))
    )
  ;; semantic db recursive load error
  ;; https://github.com/syl20bnr/spacemacs/issues/12843
  ;; (require 'semantic/db-file)

  (use-package ccc                      ; for cursor style
    :straight t
    :init
    (defun +lispy-update-cursor-style ()
      (when (and lispy-mode (evil-insert-state-p))
        (if (or (lispy-right-p) (lispy-left-p) (region-active-p))
            (progn (setq-local cursor-type '(bar . 3))
                   (ccc-set-buffer-local-cursor-color "plum1"))
          (progn (setq-local cursor-type '(bar . 3))
                 (ccc-set-buffer-local-cursor-color "green")))))
    :config
    (add-hook 'post-command-hook '+lispy-update-cursor-style))

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
  (evil-define-key 'insert lispy-mode-map (kbd "C-r") #'undo-fu-only-redo)
  (evil-define-key 'insert lispy-mode-map (kbd "C-e") 'lispy-move-end-of-line)
  (with-eval-after-load 'emacs-lisp-mode
    (evil-define-key 'normal emacs-lisp-mode-map "sl" 'lispy-goto))
  ;; (evil-define-key 'normal lispy-mode-map "b" 'sp-previous-sexp)
  ;; (evil-define-key 'normal lispy-mode-map "e" 'sp-next-sexp)
  (push '("*lispy-message*" :dedicated t :position bottom :stick t :noselect t :height 0.4) popwin:special-display-config)
  (define-key evil-normal-state-map (kbd "C-a") 'mwim-beginning-of-code-or-line)
  :config/el-patch
  (defun lispy-outline-level ()
    "Compute the outline level of the heading at point."
    (save-excursion
      (save-match-data
        (end-of-line)
        (if (re-search-backward lispy-outline nil t)
            (max (el-patch-swap (cl-count ?* (match-string 0))
                                (- (cl-count ?\; (match-string 0)) 2)) 1)
          0))))
  (defun lispy-backtick ()
    "Insert `."
    (interactive)
    (if (region-active-p)
        (el-patch-swap (lispy--surround-region "`" "`")
                       (if (memq major-mode '(clojure-mode clojurescript-mode))
                           (lispy--surround-region "`" "`")
                         (lispy--surround-region "`" "'")))
      (el-patch-swap (lispy--space-unless "\\s-\\|\\s(\\|[:?`']\\|\\\\")
                     (if (memq major-mode '(clojure-mode clojurescript-mode))
                         (lispy--space-unless "\\s-\\|\\s(\\|[:?`]\\|\\\\")
                       (lispy--space-unless "\\s-\\|\\s(\\|[:?`']\\|\\\\")))
      (insert "`")))
  (defun lispy--oneline (expr &optional ignore-comments)
    "Remove newlines from EXPR.
When IGNORE-COMMENTS is not nil, don't remove comments.
Instead keep them, with a newline after each comment."
    (lispy-mapcan-tree
     (lambda (x y)
       (cond ((el-patch-swap (equal x '(ly-raw newline))
                             (or (equal x '(ly-raw newline))
                                 (equal x '(ly-raw clojure-symbol ","))))
              y)
             ((lispy--raw-comment-p x)
              (if (null ignore-comments)
                  (progn
                    (push x lispy--oneline-comments)
                    y)
                (if (equal (car y) '(ly-raw newline))
                    (cons x y)
                  `(,x (ly-raw newline) ,@y))))
             ((and (lispy--raw-string-p x)
                   (null ignore-comments))
              (cons `(ly-raw string ,(replace-regexp-in-string "\n" "\\\\n" (cl-caddr x)))
                    y))
             (t
              (cons x y))))
     expr)))

(use-package parinfer
  :straight (:host github :repo "yqrashawn/parinfer-mode")
  :after lispy
  :disabled
  :diminish parinfer-mode
  :hook ((clojure-mode . parinfer-mode)
         (emacs-lisp-mode . parinfer-mode)
         (lisp-mode . parinfer-mode))
  :commands (parinfer-mode parinfer-mode-enable parinfer-toggle-mode)
  :init
  (defun +parinfer-hs-toggle-folding ()
    (interactive)
    (if company-my-keymap
        (company-select-previous)
      (if (region-active-p)
          (lispy-kill)
        (progn
          (hs-toggle-hiding)
          (backward-char)))))
  (setq parinfer-lighters '(" Par:I" . " Par:P"))
  (setq parinfer-display-error t)
  (setq parinfer-indent-mode-confirm nil)
  (setq parinfer-extensions
        '(defaults                 ; should be included.
           pretty-parens           ; different paren styles for different modes.
           evil                    ; If you use Evil.
           lispy ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
           ;; lispyville
           smart-tab ; C-b & C-f jump positions and smart shift with tab & S-tab.
           smart-yank))
  :config

  ;; fix conflicts between evil-iedit and parinfer-mode lispy bindings
  (add-hook
   'evil-iedit-insert-state-entry-hook
   (lambda () (when parinfer-mode
                (parinfer-mode -1))))
  (add-hook
   'evil-iedit-insert-state-exit-hook
   (lambda () (when (and (not parinfer-mode) (yq/lispy-file-p))
                (parinfer-mode 1))))
  (add-hook
   'evil-multiedit-insert-state-entry-hook
   (lambda () (when parinfer-mode
                (parinfer-mode -1))))
  (add-hook
   'evil-multiedit-insert-state-exit-hook
   (lambda () (when (and (not parinfer-mode) (yq/lispy-file-p))
                (parinfer-mode 1))))

  ;; lispy special mode cursor
  (use-package ccc                      ; for cursor style
    :straight t
    :after (parinfer)
    :init
    (defun +lispy-update-cursor-style ()
      (when (and parinfer-mode (evil-insert-state-p))
        (if (or (lispy-right-p) (lispy-left-p) (region-active-p))
            (progn (setq-local cursor-type '(bar . 6))
                   (ccc-set-buffer-local-cursor-color "plum1"))
          (progn (setq-local cursor-type '(bar . 6))
                 (ccc-set-buffer-local-cursor-color "green")))))
    :config
    (add-hook 'post-command-hook '+lispy-update-cursor-style))

  (define-key parinfer-mode-map (kbd "C-.") #'parinfer-toggle-mode)
  (define-key parinfer-mode-map (kbd "C-x @ C-x .") #'parinfer-toggle-mode)
  (evil-define-key 'insert parinfer-mode-map (kbd "C-k") '+parinfer-hs-toggle-folding)
  (define-key parinfer-mode-map (kbd "C-k") '+parinfer-hs-toggle-folding)
  (define-key parinfer-mode-map (kbd "C-x C-6 q") #'lispy-describe-inline)
  (define-key parinfer-mode-map (kbd "C-x C-6 w") #'lispy-arglist-inline)
  (define-key parinfer-mode-map (kbd "C-x C-6 z") #'lispy-left)
  (define-key parinfer-mode-map (kbd "C-x C-6 x") #'lispy-right)
  (define-key parinfer-mode-map (kbd "C-x C-6 c") #'lispy-mark-symbol)
  (define-key parinfer-mode-map (kbd "y")
    (lambda (beg end &optional region)
      (interactive (list
                    (mark)
                    (point)
                    (prefix-numeric-value
                     current-prefix-arg)))
      (if (region-active-p)
          (progn
            (kill-ring-save beg end region)
            (lispy-left 1)
            (keyboard-quit))
        (self-insert-command 1))))
  ;; (define-key parinfer-mode-map (kbd "C-k") #'lispy-kill)
  (define-key parinfer-mode-map (kbd "C-d") #'lispy-delete)
  (evil-define-key 'insert parinfer-mode-map (kbd "C-d") #'lispy-delete)
  ;; (evil-define-key 'insert parinfer-mode-map (kbd "C-a") #'lispy-move-beginning-of-line)
  (evil-define-key 'insert parinfer-mode-map (kbd "C-e") #'lispy-move-end-of-line)
  ;; (define-key parinfer-mode-map (kbd "C-a") #'lispy-move-beginning-of-line)
  (define-key parinfer-mode-map (kbd "C-e") #'lispy-move-end-of-line)
  ;; (evil-define-key 'normal parinfer-mode-map (kbd "C-a") #'lispy-move-beginning-of-line)
  (evil-define-key 'normal parinfer-mode-map (kbd "C-e") #'lispy-move-end-of-line)
  ;; (evil-define-key 'insert parinfer-mode-map (kbd "C-a") #'lispy-move-beginning-of-line)
  (evil-define-key 'insert parinfer-mode-map (kbd "C-e") #'lispy-move-end-of-line)
  (evil-define-key 'normal parinfer-mode-map "si" #'lispy-mark-symbol))

(use-package lispyville
  :straight (:host github :repo "noctuid/lispyville")
  :after (lispy)
  :diminish lispyville-mode
  :hook (lispy-mode . lispyville-mode)
  :custom
  (lispyville-motions-put-into-special t)
  (lispyville-key-theme
   '(c-w
     operators
     prettify
     text-objects
     (atom-movement t) ;; bind to WORD
     additional-movement
     commentary
     slurp/barf-lispy
     ;; wrap                               ;; M-( M-{ M-[
     ;; (additional-wrap normal visual insert)
     (additional normal visual insert) ;; M-j M-k M-J M-k M-s M-S M-r M-t M-v
     additional-insert
     escape
     mark-special
     mark-toggle))
  :config
  ;; REVIEW Delete this once https://github.com/noctuid/lispyville/pull/297 is merged
  (defadvice! +lispy--fix-lispyville-end-of-defun-a (_)
    "lispyville-end-of-defun doesn't go to the next defun when
point is already at the end of a defun, whereas
lispyville-beginning-of-defun does."
    :before #'lispyville-end-of-defun
    (when (<= (- (line-end-position)
                 (point))
              1)
      (forward-line)))
  ;; (lispyville-set-key-theme)
  (lispy-define-key lispy-mode-map "v" #'lispyville-toggle-mark-type)
  (lispy-define-key lispy-mode-map "m" #'lispy-view)
  (lispyville--define-key 'normal
    ";" (defl (lispy-comment) (evil-next-visual-line))
    "ti" #'lispyville-backward-up-list
    "ta" (defl (lispyville-up-list) (lispy-newline-and-indent))
    "tR" #'lispyville-raise-list
    "tr" #'lispy-raise-sexp
    "tt" (defl (lispyville-backward-up-list) (lispy-parens 1))
    "td" #'transpose-sexps
    "tw" #'lispy-move-up
    "tJ" #'lispy-join
    "t/" #'lispy-splice
    "ts" #'lispy-split
    "tC" #'lispy-convolute
    "txb" (lambda ()
            (interactive)
            (if (and (fboundp 'cljr-introduce-let)
                     (memq major-mode lispy-clojure-modes))
                (cljr-introduce-let)
              (lispy-bind-variable)))
    (kbd "M-RET") #'lispyville-wrap-round
    "{" (defl (lispyville-insert-at-beginning-of-list 1) (insert " ") (backward-char))
    "}" (defl (lispyville-insert-at-end-of-list 1) (insert "")))
  (lispyville--define-key 'insert
    (kbd "M-RET") #'lispyville-wrap-round
    (kbd "C-r") #'lispyville-backward-up-list)
  (lispyville--define-key 'visual
    (kbd "(") #'lispyville-wrap-round
    (kbd "{") #'lispyville-wrap-braces
    (kbd "[") #'lispyville-wrap-brackets)

  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
  (define-key evil-inner-text-objects-map "g" 'evil-inner-buffer)
  (define-key evil-inner-text-objects-map "P" 'evil-pasted)
  (define-key evil-inner-text-objects-map "f" 'evil-textobj-anyblock-inner-block)
  (define-key evil-outer-text-objects-map "f" 'evil-textobj-anyblock-a-block))

(use-package eval-sexp-fu
  :straight t
  :commands (eval-sexp-fu-flash-mode)
  :hook (emacs-lisp-mode . eval-sexp-fu-flash-mode))


(use-package highlight-defined
  :straight t
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package highlight-quoted
  :straight t
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(use-package ipretty
  :straight t
  :hook (emacs-lisp-mode . ipretty-mode))

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

(use-package lisp-extra-font-lock
  :straight (:host github :repo "Lindydancer/lisp-extra-font-lock")
  :hook ((emacs-lisp-mode) . lisp-extra-font-lock-mode))

(use-package highlight-function-calls
  :straight t
  :hook ((emacs-lisp-mode) . highlight-function-calls-mode))

(use-package parinfer-rust-mode
  :straight (:host github :repo "justinbarclay/parinfer-rust-mode")
  :disabled
  :hook ((clojure-mode . parinfer-rust-mode)
         (emacs-lisp-mode . parinfer-rust-mode)
         (lisp-mode . parinfer-rust-mode)))

(use-package highlight-stages
  :straight t
  :disabled
  :hook (emacs-lisp-mode . highlight-stages-mode))

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

(use-package easy-escape
  ;; make elisp regexp more readable
  :straight t
  :hook (emacs-lisp-mode . easy-escape-minor-mode))

(use-package symex
  :straight t
  :disabled
  :bind ("s-'" . symex-mode-interface)
  :custom
  (symex-modal-backend 'evil)
  :config
  (symex-initialize)
  ;; (dolist (mode-name symex-lisp-modes)
  ;;   (let ((mode-hook (intern (concat (symbol-name mode-name)
  ;;                                    "-hook"))))
  ;;     (add-hook mode-hook 'symex-mode)))
  )
