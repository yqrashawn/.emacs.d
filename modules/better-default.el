(customize-set-variable 'inhibit-startup-screen t)
(customize-set-variable 'inhibit-startup-message t)
(defvar dotspacemacs-auto-save-file-location 'cache
  "Location where to auto-save files. Possible values are `original' to
auto-save the file in-place, `cache' to auto-save the file to another
file stored in the cache directory and `nil' to disable auto-saving.")
(defconst spacemacs-auto-save-directory
  (expand-file-name (concat yq-emacs-cache-dir "auto-save/"))
  "Spacemacs auto-save directory")
(defconst spacemacs-assets-directory
  (expand-file-name (concat user-emacs-directory "assets/"))
  "Spacemacs assets directory.")
(defconst spacemacs-test-directory
  (expand-file-name (concat user-emacs-directory "tests/"))
  "Spacemacs tests directory.")
(defconst user-home-directory
  (expand-file-name "~/")
  "User home directory (~/).")


(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)
(setq help-window-select 't)
(setq compilation-scroll-output 'first-error)
(setq ffap-machine-p-known 'reject)
(setq dired-dwim-target t)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)
(xterm-mouse-mode 1)
(setq initial-major-mode 'text-mode)
(setq longlines-show-hard-newlines t)
(setq delete-by-moving-to-trash t)
(setq-default fill-column 80)
(setq abbrev-file-name (concat yq-emacs-cache-dir "abbrev_defs"))
(setq save-interprogram-paste-before-kill t)
(setq-default sentence-end-double-space nil)
(with-eval-after-load 'comint
  (define-key comint-mode-map (kbd "C-d") nil))
(setq window-combination-resize t)
(setq column-number-mode t)
(blink-cursor-mode 0)
(setq x-underline-at-descent-line t)
;; don't let the cursor go into minibuffer prompt
;; Tip taken from Xah Lee: http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
(setq initial-scratch-message nil)
(setq make-backup-files nil)
(setq auto-save-default (not (null dotspacemacs-auto-save-file-location)))
(setq auto-save-list-file-prefix (concat spacemacs-auto-save-directory))

;; Hack to fix a bug with tabulated-list.el
;; see: http://redd.it/2dgy52
(defun tabulated-list-revert (&rest ignored)
  "The `revert-buffer-function' for `tabulated-list-mode'.
It runs `tabulated-list-revert-hook', then calls `tabulated-list-print'."
  (interactive)
  (unless (derived-mode-p 'tabulated-list-mode)
    (error "The current buffer is not in Tabulated List mode"))
  (run-hooks 'tabulated-list-revert-hook)
  ;; hack is here
  ;; (tabulated-list-print t)
  (tabulated-list-print))

(setq-default indent-tabs-mode nil
              tab-width 2)
(fset 'yes-or-no-p 'y-or-n-p)

;; Auto-save file
(setq auto-save-default (not (null dotspacemacs-auto-save-file-location)))
(setq auto-save-list-file-prefix (concat spacemacs-auto-save-directory))

;; always save TRAMP URLs to cache directory no matter what is the value
;; of `dotspacemacs-auto-save-file-location'

;; (let ((autosave-dir (concat spacemacs-auto-save-directory "dist/")))
;;   (setq auto-save-file-name-transforms
;;         `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,autosave-dir  t)))
;;   (unless (or (file-exists-p autosave-dir)
;;               (null dotspacemacs-auto-save-file-location))
;;     (make-directory autosave-dir t)))


;; Choose auto-save location
(cl-case dotspacemacs-auto-save-file-location
  (cache (let ((autosave-dir (concat spacemacs-auto-save-directory "site/")))
           (add-to-list 'auto-save-file-name-transforms
                        `(".*" ,autosave-dir t) 'append)
           (unless (file-exists-p autosave-dir)
             (make-directory autosave-dir t))))
  (original (setq auto-save-visited-file-name t))
  (_ (setq auto-save-default nil
           auto-save-list-file-prefix nil)))

;; remove annoying ellipsis when printing sexp in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;; cache files
;; (setq tramp-persistency-file-name (concat spacemacs-cache-directory "tramp"))

;; seems pointless to warn. There's always undo.
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; remove prompt if the file is opened in other clients
(defun server-remove-kill-buffer-hook ()
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))
(add-hook 'server-visit-hook 'server-remove-kill-buffer-hook)

(add-hook 'prog-mode-hook 'hs-minor-mode)

(defun yq/edit-dotfile ()
  (interactive)
  (find-file-existing yq-emacs-dotfile-dir))

;; C-h key as BS
(keyboard-translate ?\C-h ?\C-?)
(global-set-key [(control ?h)] 'delete-backward-char)

(defvar yq-indent-sensitive-modes
  '(asm-mode
    coffee-mode
    elm-mode
    haml-mode
    haskell-mode
    slim-mode
    makefile-mode
    makefile-bsdmake-mode
    makefile-gmake-mode
    makefile-imake-mode
    python-mode
    yaml-mode)
  "Modes for which auto-indenting is suppressed.")

(defun yq/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode yq-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (evil-indent (point-min) (point-max))
          (message "Indented buffer.")))
      (whitespace-cleanup))))

(defun yq/kill-this-buffer (&optional arg)
  "Kill the current buffer.
If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal '(4) arg)
        (kill-buffer-and-window)
      (kill-buffer))))

(defun yq/delete-window (&optional arg)
  "Delete the current window.
If the universal prefix argument is used then kill the buffer too."
  (interactive "P")
  (if (equal '(4) arg)
      (kill-buffer-and-window)
    (delete-window)))

(defun yq/backward-kill-word-or-region (&optional arg)
  "Calls `kill-region' when a region is active and
`backward-kill-word' otherwise. ARG is passed to
`backward-kill-word' if no region is active."
  (interactive "p")
  (if (region-active-p)
      ;; call interactively so kill-region handles rectangular selection
      ;; correctly (see https://github.com/syl20bnr/yq/issues/3278)
      (call-interactively #'kill-region)
    (backward-kill-word arg)))

(global-set-key (kbd "C-w") 'yq/backward-kill-word-or-region)

(use-package mwim
  :straight t
  :config
  (global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (global-set-key (kbd "C-e") 'mwim-end-of-code-or-line))

(add-hook 'hs-minor-mode-hook '(lambda () (diminish 'hs-minor-mode)))
(add-hook 'auto-revert-mode-hook '(lambda () (diminish 'auto-revert-mode)))

(electric-indent-mode)

(yq/add-toggle visual-line :mode visual-line-mode)
(setq org-confirm-babel-evaluate nil)
(setq vc-follow-symlinks nil)
(yq/add-toggle show-paren :mode show-paren-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)

(use-package windmove
  :bind( ("C-x 7 w h" . 'windmove-left)
         ("C-x 7 w l" . 'windmove-right)
         ("C-x 7 w j" . 'windmove-down)
         ("C-x 7 w k" . 'windmove-up)))

(use-package winner
  :init
  (winner-mode t)
  (setq spacemacs/winner-boring-buffers '("*Completions*"
                                          "*Compile-Log*"
                                          "*inferior-lisp*"
                                          "*Fuzzy Completions*"
                                          "*Apropos*"
                                          "*Help*"
                                          "*cvs*"
                                          "*Buffer List*"
                                          "*Ibuffer*"
                                          "*esh command on file*"
                                          ))
  (setq winner-boring-buffers
        (append winner-boring-buffers spacemacs/winner-boring-buffers))
  (winner-mode t))


(with-eval-after-load 'hi-lock
  (diminish 'hi-lock-mode))

(use-package linum
  :init
  (progn
    (setq linum-format "%4d")
    (yq/add-toggle line-numbers :mode linum-mode)
    (spacemacs/set-leader-keys "tn" 'yq/toggle-line-numbers)
    (defun spacemacs//linum-update-window-scale-fix (win)
      "Fix linum for scaled text in the window WIN."
      (when (display-multi-font-p)
        (unless (boundp 'text-scale-mode-step)
          (setq window-initial-margins (window-margins win)))
        (set-window-margins win
                            (ceiling (* (if (boundp 'text-scale-mode-step)
                                            (expt text-scale-mode-step
                                                  text-scale-mode-amount)
                                          1)
                                        (or (car (if (boundp 'window-initial-margins)
                                                     window-initial-margins
                                                   (window-margins win)))
                                            1))))))
    (advice-add #'linum-update-window
                :after #'spacemacs//linum-update-window-scale-fix)))

(use-package savehist
  :init
  ;; Minibuffer history
  (setq savehist-file (concat spacemacs-cache-directory "savehist")
        enable-recursive-minibuffers t ; Allow commands in minibuffers
        history-length 1000
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
        savehist-autosave-interval 60)
  (savehist-mode t))

(use-package recentf
  :defer t
  :init
  ;; lazy load recentf
  (add-hook 'find-file-hook (lambda () (unless recentf-mode
                                         (recentf-mode)
                                         (recentf-track-opened-file))))
  (setq recentf-save-file (concat spacemacs-cache-directory "recentf")
        recentf-max-saved-items 1000
        recentf-auto-cleanup 'never
        recentf-auto-save-timer (run-with-idle-timer 600 t
                                                     'recentf-save-list))
  :config
  (add-to-list 'recentf-exclude
               (file-truename spacemacs-cache-directory))
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'"))

(use-package saveplace
  :init
  (if (fboundp 'save-place-mode)
      ;; Emacs 25 has a proper mode for `save-place'
      (save-place-mode)
    (setq save-place t))
  ;; Save point position between sessions
  (setq save-place-file (concat spacemacs-cache-directory "places")))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        ;; don't screw special buffers
        uniquify-ignore-buffers-re "^\\*"))

(use-package whitespace
  :defer t
  :diminish whitespace-mode
  :diminish whitespace-global-modes
  :init
  (setq spacemacs-show-trailing-whitespace t)
  (defun spacemacs//show-trailing-whitespace ()
    (when spacemacs-show-trailing-whitespace
      (set-face-attribute 'trailing-whitespace nil
                          :background
                          (face-attribute 'font-lock-comment-face
                                          :foreground))
      (setq show-trailing-whitespace 1)))
  (add-hook 'prog-mode-hook 'spacemacs//show-trailing-whitespace)

  (yq/add-toggle whitespace :mode whitespace-mode)
  (spacemacs/set-leader-keys "tw" 'yq/toggle-whitespace)

  (defun spacemacs//set-whitespace-style-for-diff ()
    "Whitespace configuration for `diff-mode'"
    (setq-local whitespace-style '(face
                                   tabs
                                   tab-mark
                                   spaces
                                   space-mark
                                   trailing
                                   indentation::space
                                   indentation::tab
                                   newline
                                   newline-mark)))
  (add-hook 'diff-mode-hook 'whitespace-mode)
  (add-hook 'diff-mode-hook 'spacemacs//set-whitespace-style-for-diff)
  :config
  (set-face-attribute 'whitespace-space nil
                      :background nil
                      :foreground (face-attribute 'font-lock-warning-face
                                                  :foreground))
  (set-face-attribute 'whitespace-tab nil
                      :background nil)
  (set-face-attribute 'whitespace-indentation nil
                      :background nil))

(use-package bookmark
  :defer t
  :init
  (setq bookmark-default-file (concat spacemacs-cache-directory "bookmarks")
        ;; autosave each change
        bookmark-save-flag 1)
  (spacemacs/set-leader-keys "fb" 'bookmark-jump))

(use-package popwin
  :straight t
  :config
  (popwin-mode 1)
  (spacemacs/set-leader-keys "bm" 'popwin:messages)
  (spacemacs/set-leader-keys "bc" 'popwin:close-popup-window)
  (setq popwin:special-display-config nil)

  ;; https://github.com/m2ym/popwin-el/tree/95dea14c60019d6cccf9a3b33e0dec4e1f22c304#special-display-config
  ;; buffers that we manage
  (push '("*Help*"                 :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
  (push '("*Backtrace*"            :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
  (push '("*Warnings*"            :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
  (push '("*compilation*"          :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
  (push '("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect t            ) popwin:special-display-config)
  (push '("*Async Shell Command*"  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
  (push '(" *undo-tree*"           :dedicated t :position right  :stick t :noselect nil :width   60) popwin:special-display-config)
  (push '("*undo-tree Diff*"       :dedicated t :position bottom :stick t :noselect nil :height 0.3) popwin:special-display-config)
  (push '("*ert*"                  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
  (push '("*grep*"                 :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
  (push '("*nosetests*"            :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
  (push '("^\*WoMan.+\*$" :regexp t             :position bottom                                   ) popwin:special-display-config)
  (define-key evil-normal-state-map (kbd "C-z") popwin:keymap))

(setq standard-indent 2)

(defvar dotspacemacs-scratch-mode 'text-mode
  "Default major mode of the scratch buffer.")

(defun spacemacs/switch-to-scratch-buffer (&optional arg)
  "Switch to the `*scratch*' buffer, creating it first if needed.
if prefix argument ARG is given, switch to it in an other, possibly new window."
  (interactive "P")
  (let ((exists (get-buffer "*scratch*")))
    (if arg
        (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
      (switch-to-buffer (get-buffer-create "*scratch*")))
    (when (and (not exists)
               (not (eq major-mode dotspacemacs-scratch-mode))
               (fboundp dotspacemacs-scratch-mode))
      (funcall dotspacemacs-scratch-mode))))

(evil-leader/set-key "bs" 'spacemacs/switch-to-scratch-buffer)

;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
   Dwim means: region, org-src-block, org-subtree, or defun,
   whichever applies first. Narrowing to org-src-block actually
   calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is
already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if you
         ;; don't want it.
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

;; replace downcase region
(global-set-key "\C-x\C-l" 'narrow-or-widen-dwim)

(defun spacemacs/kill-other-buffers (&optional arg)
  "Kill all other buffers.
If the universal prefix argument is used then will the windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                             (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))))
(evil-leader/set-key (kbd "b C-d") 'spacemacs/kill-other-buffers)
