(setq *is-a-mac* (eq system-type 'darwin))
(setq *win64* (eq system-type 'windows-nt))
(setq *cygwin* (eq system-type 'cygwin))
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )
(setq *emacs24* (and (not (featurep 'xemacs)) (or (>= emacs-major-version 24))) )
(setq *emacs25* (and (not (featurep 'xemacs)) (or (>= emacs-major-version 25))) )
(setq *no-memory* (cond
                   (*is-a-mac*
                    (< (string-to-number (nth 1 (split-string (shell-command-to-string "sysctl hw.physmem")))) 4000000000))
                   (*linux* nil)
                   (t nil)))

;; emacs 24.3-
(setq *emacs24old*  (or (and (= emacs-major-version 24) (= emacs-minor-version 3))
                        (not *emacs24*)))

(customize-set-variable 'inhibit-startup-screen t)
(customize-set-variable 'inhibit-startup-message t)
(customize-set-variable 'inhibit-startup-echo-area-message t)
(setq confirm-nonexistent-file-or-buffer nil)
(setq kmacro-ring-max 30)

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

(setq user-mail-address "namy.19@gmail.com")

(setq source-directory (concat user-home-directory "emacs"))
(size-indication-mode t)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)
;; (global-prettify-symbols-mode +1)
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
(use-package abbrev
  :defer t
  :ensure nil
  :diminish abbrev-mode
  :custom
  (abbrev-file-name (concat user-emacs-directory "abbrev_defs"))
  (abbrev-mode 1)
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))
(setq save-interprogram-paste-before-kill t)
(setq confirm-kill-processes nil)
(setq-default sentence-end-double-space nil)
(with-eval-after-load 'comint
  (define-key comint-mode-map (kbd "C-d") nil))
(setq window-combination-resize t)
(setq column-number-mode t)
(blink-cursor-mode -1)
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
(add-hook 'org-mode-hook 'hs-minor-mode)

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
  :bind(("C-x 7 w h" . 'windmove-left)
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
    (yq/add-toggle line-numbers :mode display-line-numbers-mode)
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
  :straight t
  :init
  (setq recentf-keep '(file-remote-p file-readable-p))
  ;; lazy load recentf
  (add-hook 'find-file-hook
            (lambda ()
              (unless recentf-mode
                (recentf-mode)
                (recentf-track-opened-file))))
  (setq recentf-save-file (concat user-emacs-directory "recentf")
        recentf-max-saved-items 10000
        recentf-auto-cleanup 'never
        recentf-auto-save-timer (run-with-idle-timer 600 t
                                                     'recentf-save-list))
  :config
  (run-at-time nil (* 5 60) 'recentf-save-list)
  (add-to-list 'recentf-exclude
               (file-truename spacemacs-cache-directory))
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
  (add-to-list 'recentf-exclude "/private/var/folders/")
  (add-to-list 'recentf-exclude "/var/folders/")
  (add-to-list 'recentf-exclude "/var/tmp/")
  (add-to-list 'recentf-exclude (expand-file-name (concat user-emacs-directory "recentf")))
  (add-to-list 'recentf-exclude "/tmp/")
  (add-to-list 'recentf-exclude "\\indium-eval-.*"))

;; (use-package recentf-ext
;;   :straight t
;;   :after recentf)

;; saveplace remembers your location in a file when saving files
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
  (push '("*cider-error*"                       :dedicated t   :position bottom :stick t    :noselect t   :height 0.4) popwin:special-display-config)
  (push '("\*rg-scan-async\*.*"      :regexp t  :dedicated t   :position bottom :stick nil  :noselect t   :height 0.1) popwin:special-display-config)
  (push '("*Contents*"                          :dedicated t   :position bottom :stick t    :noselect t   :height 0.4)   popwin:special-display-config)
  (push '("*Occur*"                             :dedicated t   :position bottom :stick t    :noselect nil :height 0.4)   popwin:special-display-config)
  (push '("^magit-process:\ .*"      :regexp t  :dedicated t   :position bottom :stick t    :noselect nil :height 0.4)   popwin:special-display-config)
  (push '("\*helpful\ .*\*"          :regexp t  :dedicated nil :position bottom :stick t    :noselect t   :height 0.4)   popwin:special-display-config)
  (push '("*Help*"                              :dedicated nil :position bottom :stick t    :noselect t   :height 0.4) popwin:special-display-config)
  (push '("*Backtrace*"                         :dedicated nil :position bottom :stick t    :noselect t   :height 0.4)   popwin:special-display-config)
  (push '("*Warnings*"                          :dedicated t   :position bottom :stick t    :noselect t   :height 0.4)   popwin:special-display-config)
  (push '("*HTTP Response*"                     :dedicated t   :position bottom :stick t    :noselect t   :height 0.4)   popwin:special-display-config)
  (push '("*compilation*"                       :dedicated t   :position bottom :stick t    :noselect t   :height 0.4)   popwin:special-display-config)
  (push '("*Shell Command Output*"              :dedicated t   :position bottom :stick t    :noselect t   :height 0.4)   popwin:special-display-config)
  (push '("*prettier errors*"                   :dedicated t   :position bottom :stick nil  :noselect t   :height 0.4)   popwin:special-display-config)
  (push '("*Async Shell Command*"               :dedicated t   :position bottom :stick t    :noselect t              )   popwin:special-display-config)
  (push '("*undo-tree*"                         :dedicated t   :position right  :stick t    :noselect nil :width   60)   popwin:special-display-config)
  (push '("*undo-tree Diff*"                    :dedicated t   :position bottom :stick t    :noselect nil :height 0.3)   popwin:special-display-config)
  (push '("*ert*"                               :dedicated t   :position bottom :stick t    :noselect t              )   popwin:special-display-config)
  (push '("*grep*"                              :dedicated t   :position bottom :stick t    :noselect nil            )   popwin:special-display-config)
  (push '("*nosetests*"                         :dedicated t   :position bottom :stick t    :noselect nil            )   popwin:special-display-config)
  (push '("^\*WoMan.+\*$"           :regexp t   :dedicated t   :position bottom             :noselect t              )   popwin:special-display-config))
;; (define-key evil-normal-state-map (kbd "C-z") popwin:keymap)

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
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
  (when (equal '(4) arg) (delete-other-windows)))

(defun kill-all-buffers ()
  "kill all buffers"
  (interactive)
  (mapc 'kill-buffer (buffer-list (current-buffer))))

(global-set-key (kbd "s-K") 'kill-all-buffers)
(evil-leader/set-key (kbd "b C-d") 'spacemacs/kill-other-buffers)

(setq require-final-newline nil)
(setq mode-require-final-newline nil)

(use-package restart-emacs
  :straight t
  :commands (restart-emacs)
  :init
  (spacemacs/set-leader-keys "qq" 'save-buffers-kill-emacs)
  (spacemacs/set-leader-keys "qr" 'restart-emacs))

(yq/add-toggle hl-line :mode hl-line-mode)
(spacemacs/set-leader-keys "tL" 'yq/toggle-hl-line)
(spacemacs/set-leader-keys "Ts" 'load-theme)

(yq/add-toggle auto-fill :mode auto-fill-mode)
(spacemacs/set-leader-keys "tf" 'yq/toggle-auto-fill)

;; (use-package edit-server
;;   :straight t
;;   :config (edit-server-start))
;; (use-package atomic-chrome
;;   :straight t
;;   :config (atomic-chrome-start-server))
(setq-default bidi-display-reordering nil)

;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
(setq auto-window-vscroll nil)

(defun spacemacs/rename-file (filename &optional new-filename)
  "Rename FILENAME to NEW-FILENAME.

When NEW-FILENAME is not specified, asks user for a new name.

Also renames associated buffer (if any exists), invalidates
projectile cache when it's possible and update recentf list."
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let* ((buffer (find-buffer-visiting filename))
           (short-name (file-name-nondirectory filename))
           (new-name (if new-filename new-filename
                       (read-file-name
                        (format "Rename %s to: " short-name)))))
      (cond ((get-buffer new-name)
             (error "A buffer named '%s' already exists!" new-name))
            (t
             (let ((dir (file-name-directory new-name)))
               (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                 (make-directory dir t)))
             (rename-file filename new-name 1)
             (when buffer
               (kill-buffer buffer)
               (find-file new-name))
             (when (fboundp 'recentf-add-file)
               (recentf-add-file new-name)
               (recentf-remove-if-non-kept filename))
             (when (and (configuration-layer/package-used-p 'projectile)
                        (projectile-project-p))
               (call-interactively #'projectile-invalidate-cache))
             (message "File '%s' successfully renamed to '%s'" short-name (file-name-nondirectory new-name)))))))

;; from magnars
(defun spacemacs/rename-current-buffer-file (&optional arg)
  "Rename the current buffer and the file it is visiting.
If the buffer isn't visiting a file, ask if it should
be saved to a file, or just renamed.

If called without a prefix argument, the prompt is
initialized with the current filename."
  (interactive "P")
  (let* ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (and filename (file-exists-p filename))
        ;; the buffer is visiting a file
        (let* ((dir (file-name-directory filename))
               (new-name (read-file-name "New name: " (if arg dir filename))))
          (cond ((get-buffer new-name)
                 (error "A buffer named '%s' already exists!" new-name))
                (t
                 (let ((dir (file-name-directory new-name)))
                   (when (and (not (file-exists-p dir))
                              (yes-or-no-p
                               (format "Create directory '%s'?" dir)))
                     (make-directory dir t)))
                 (rename-file filename new-name 1)
                 (rename-buffer new-name)
                 (set-visited-file-name new-name)
                 (set-buffer-modified-p nil)
                 (when (fboundp 'recentf-add-file)
                   (recentf-add-file new-name)
                   (recentf-remove-if-non-kept filename))
                 (when (and (configuration-layer/package-used-p 'projectile)
                            (projectile-project-p))
                   (call-interactively #'projectile-invalidate-cache))
                 (message "File '%s' successfully renamed to '%s'"
                          name (file-name-nondirectory new-name)))))
      ;; the buffer is not visiting a file
      (let ((key))
        (while (not (memq key '(?s ?r)))
          (setq key (read-key (propertize
                               (format
                                (concat "Buffer '%s' is not visiting a file: "
                                        "[s]ave to file or [r]ename buffer?")
                                name) 'face 'minibuffer-prompt)))
          (cond ((eq key ?s)            ; save to file
                 ;; this allows for saving a new empty (unmodified) buffer
                 (unless (buffer-modified-p) (set-buffer-modified-p t))
                 (save-buffer))
                ((eq key ?r)            ; rename buffer
                 (let ((new-name (read-string "New buffer name: ")))
                   (while (get-buffer new-name)
                     ;; ask to rename again, if the new buffer name exists
                     (if (yes-or-no-p
                          (format (concat "A buffer named '%s' already exists: "
                                          "Rename again?") new-name))
                         (setq new-name (read-string "New buffer name: "))
                       (keyboard-quit)))
                   (rename-buffer new-name)
                   (message "Buffer '%s' successfully renamed to '%s'"
                            name new-name)))
                ;; ?\a = C-g, ?\e = Esc and C-[
                ((memq key '(?\a ?\e)) (keyboard-quit))))))))

(defun spacemacs/delete-file (filename &optional ask-user)
  "Remove specified file or directory.

Also kills associated buffer (if any exists) and invalidates
projectile cache when it's possible.

When ASK-USER is non-nil, user will be asked to confirm file
removal."
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let ((buffer (find-buffer-visiting filename)))
      (when buffer
        (kill-buffer buffer)))
    (when (or (not ask-user)
              (yes-or-no-p "Are you sure you want to delete this file? "))
      (delete-file filename)
      (when (and (configuration-layer/package-used-p 'projectile)
                 (projectile-project-p))
        (call-interactively #'projectile-invalidate-cache)))))

(defun spacemacs/delete-file-confirm (filename)
  "Remove specified file or directory after users approval.

FILENAME is deleted using `spacemacs/delete-file' function.."
  (interactive "f")
  (funcall-interactively #'spacemacs/delete-file filename t))

;; from magnars
(defun spacemacs/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (when (and (configuration-layer/package-used-p 'projectile)
                   (projectile-project-p))
          (call-interactively #'projectile-invalidate-cache))
        (message "File '%s' successfully removed" filename)))))

;; from magnars
(defun spacemacs/sudo-edit (&optional arg)
  (interactive "P")
  ;; (require 'tramp)
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "File: ")
                 buffer-file-name)))
    (find-file
     (if (not (tramp-tramp-file-p fname))
         (concat "/sudo:root@localhost:" fname)
       (with-parsed-tramp-file-name fname parsed
                                    (when (equal parsed-user "root")
                                      (error "Already root!"))
                                    (let* ((new-hop (tramp-make-tramp-file-name parsed-method
                                                                                parsed-user
                                                                                parsed-host
                                                                                nil
                                                                                parsed-hop
                                                                                ))
                                           (new-hop (substring new-hop 1 -1))
                                           (new-hop (concat new-hop "|"))
                                           (new-fname (tramp-make-tramp-file-name "sudo"
                                                                                  "root"
                                                                                  parsed-host
                                                                                  parsed-localname
                                                                                  new-hop)))
                                      new-fname))))))
(use-package ssh-config-mode
  :straight t
  :mode ("~/.ssh/config". ssh-config-mode))

(defun yq/fix-evil-state-bug ()
  ;; https://github.com/emacs-evil/evil/issues/301
  (evil-insert-state)
  (evil-normal-state))

(use-package gitconfig-mode
  :straight t
  :defer t)
(use-package gitignore-mode
  :straight t
  :defer t)
(use-package gitattributes-mode
  :straight t
  :defer t)

(use-package autoinsert
  :straight t
  :init
  ;; Don't want to be prompted before insertion:
  (setq auto-insert-query nil)
  (setq auto-insert-directory (concat user-emacs-directory ".templates/"))
  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1)
  :config
  (defun autoinsert-yas-expand()
    "Replace text in yasnippet template."
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))
  (define-auto-insert "\\.html?$" "template.html")
  (define-auto-insert "\\.\\(js\\|jsx\\)$" ["template.js" autoinsert-yas-expand])
  (define-auto-insert "\\.\\(ts\\|tsx\\)$" ["template.ts" autoinsert-yas-expand])
  (define-auto-insert "\\.el$" ["template.el" autoinsert-yas-expand]))

(defun spacemacs/scale-up-or-down-font-size (direction)
  "Scale the font. If DIRECTION is positive or zero the font is scaled up,
otherwise it is scaled down."
  (interactive)
  (let ((scale 0.5))
    (if (eq direction 0)
        (text-scale-set 0)
      (if (< direction 0)
          (text-scale-decrease scale)
        (text-scale-increase scale)))))

(defun spacemacs/scale-up-font ()
  "Scale up the font."
  (interactive)
  (spacemacs/scale-up-or-down-font-size 1))

(defun spacemacs/scale-down-font ()
  "Scale up the font."
  (interactive)
  (spacemacs/scale-up-or-down-font-size -1))

(defun spacemacs/reset-font-size ()
  "Reset the font size."
  (interactive)
  (spacemacs/scale-up-or-down-font-size 0))

(global-set-key (kbd "s-=") 'spacemacs/scale-up-font)
(global-set-key (kbd "s--") 'spacemacs/scale-down-font)

(use-package info
  :straight t
  :commands (info)
  :config
  (define-key Info-mode-map "s" nil)
  (define-key Info-mode-map "ss" 'Info-search)
  (define-key Info-mode-map "sj" 'counsel-recentf)
  (define-key Info-mode-map (kbd "s SPC") 'counsel-M-x)
  (define-key Info-mode-map "sc" 'yq/delete-window)
  (define-key Info-mode-map "sk" 'yq/kill-this-buffer)
  (evil-define-key 'normal
    "s" nil
    "sj" 'counsel-recentf
    (kbd "s SPC") 'counsel-M-x))

(setq confirm-kill-emacs nil)
(spacemacs/set-leader-keys "xdw" 'delete-trailing-whitespace)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

(defun switch-to-nth-buffer (n)
  "Switches to nth most recent buffer. Ignores a bunch of stuff."
  (catch 'tag
    (mapcar (lambda (b)
              (unless
                  (or
                   (minibufferp b)
                   (string-match "^ " (buffer-name b))
                   (string-match "\*" (buffer-name b))
                   (equal b (current-buffer)))
                (if (= n 1)
                    (progn
                      (switch-to-buffer b)
                      (throw 'tag nil))
                  (setq n (- n 1)))))
            (buffer-list))))

(defun switch-to-most-recent-buffer ()
  (interactive)
  (switch-to-nth-buffer 1))
(defun switch-to-second-most-recent-buffer ()
  (interactive)
  (switch-to-nth-buffer 2))
(defun switch-to-third-most-recent-buffer ()
  (interactive)
  (switch-to-nth-buffer 3))

;;fast switching between two buffers
;; (define-key evil-normal-state-map (kbd "<tab>") 'switch-to-most-recent-buffer)

;;fast switching between three buffers
(define-key evil-normal-state-map (kbd "<C-tab>") 'switch-to-second-most-recent-buffer)
(define-key evil-normal-state-map (kbd "<C-s-tab>") 'switch-to-third-most-recent-buffer)

(add-hook 'edebug-mode-hook 'yq/toggle-show-paren-off)

(defconst evil-collection-edebug-maps
  '(edebug-mode-map
    edebug-x-instrumented-function-list-mode-map
    edebug-x-breakpoint-list-mode-map))

(defun evil-collection-edebug-setup ()
  "Set up `evil' bindings for `edebug'."
  (interactive)
  (evil-set-initial-state 'edebug-mode 'normal)

  (add-hook 'edebug-mode-hook #'evil-normalize-keymaps)

  (define-key edebug-mode-map "g" nil)
  (define-key edebug-mode-map "G" nil)

  ;; FIXME: Seems like other minor modes will readily clash with `edebug'.
  ;; `lispyville' and `edebug' 's' key?
  (evil-define-key 'normal edebug-mode-map
    ;; control
    "s" 'edebug-step-mode
    "n" 'edebug-next-mode
    "go" 'edebug-go-mode
    "gO" 'edebug-Go-nonstop-mode
    "t" 'edebug-trace-mode
    "T" 'edebug-Trace-fast-mode
    "c" 'edebug-continue-mode
    "C" 'edebug-Continue-fast-mode

    "f" 'edebug-forward-sexp
    "H" 'edebug-goto-here
    "I" 'edebug-instrument-callee
    "i" 'edebug-step-in
    "o" 'edebug-step-out

    ;; quit
    "q" 'top-level
    "Q" 'edebug-top-level-nonstop
    "a" 'abort-recursive-edit
    "S" 'edebug-stop

    ;; breakpoints
    "b" 'edebug-set-breakpoint
    "u" 'edebug-unset-breakpoint
    "B" 'edebug-next-breakpoint
    "x" 'edebug-set-conditional-breakpoint
    "X" 'edebug-set-global-break-condition

    ;; evaluation
    "r" 'edebug-previous-result
    "e" 'edebug-eval-expression
    (kbd "C-x C-e") 'edebug-eval-last-sexp
    "EL" 'edebug-visit-eval-list

    ;; views
    "WW" 'edebug-where
    "p" 'edebug-bounce-point
    "P" 'edebug-view-outside ;; same as v
    "WS" 'edebug-toggle-save-windows

    ;; misc
    "g?" 'edebug-help
    "d" 'edebug-backtrace

    "-" 'negative-argument

    ;; statistics
    "=" 'edebug-temp-display-freq-count

    ;; GUD bindings
    (kbd "C-c C-s") 'edebug-step-mode
    (kbd "C-c C-n") 'edebug-next-mode
    (kbd "C-c C-c") 'edebug-go-mode

    (kbd "C-x SPC") 'edebug-set-breakpoint
    (kbd "C-c C-d") 'edebug-unset-breakpoint
    (kbd "C-c C-t") (lambda () (interactive) (edebug-set-breakpoint t))
    (kbd "C-c C-l") 'edebug-where)

  (with-eval-after-load 'edebug-x
    (evil-define-key 'normal edebug-x-instrumented-function-list-mode-map
      "E" 'edebug-x-evaluate-function
      "Q" 'edebug-x-clear-data
      (kbd "<return>") 'edebug-x-find-function
      "q" 'quit-window)
    (evil-define-key 'normal edebug-x-breakpoint-list-mode-map
      (kbd "<return>") 'edebug-x-visit-breakpoint
      "x" 'edebug-x-kill-breakpoint
      "Q" 'edebug-x-clear-data
      "q" 'quit-window)))

;; (add-hook 'edebug-mode-hook 'evil-collection-edebug-setup)

(use-package carbon-now-sh
  :commands (carbon-now-sh)
  :straight (:host github :repo "veelenga/carbon-now-sh.el"))

;; If you tramp is hanging, you can uncomment below line.
;; (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

(use-package keyfreq
  :straight t
  :init
  (defun turnon-keyfreq-mode ()
    (interactive)
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1))

  (defun turnoff-keyfreq-mode ()
    (interactive)
    (keyfreq-mode -1)
    (keyfreq-autosave-mode -1))
  (setq keyfreq-excluded-commands
        '(evil-next-visual-line
          evil-previous-visual-line
          evil-next-visual-line
          evil-previous-line))
  (turnon-keyfreq-mode)
  :config
  (unless (file-exists-p (file-truename keyfreq-file))
    (with-temp-buffer
      (insert "()")
      (write-file (file-truename keyfreq-file)))))

;; updated line number every second
(setq linum-delay t)

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq history-delete-duplicates t)

;; {{ @see http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.\nWith a prefix ARG prompt for a file to visit.\nWill also prompt for a file to visit if current\nbuffer is not visiting a file.\nYou may insert below line into ~/.authinfo.gpg to type less:\nmachine 127.0.0.1 login root password ****** port sudo\nSee \"Reusing passwords for several connections\" from INFO.\n"
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@127.0.0.1:"
                         (read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:@127.0.0.1:"
                                 buffer-file-name))))

(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (if (and (not (and buffer-file-name
                     (file-writable-p buffer-file-name)))
           ;; sudo edit only physical file
           buffer-file-name
           ;; sudo edit only /etc/**/*
           (string-match-p "^/etc/" buffer-file-name))
      (find-alternate-file (concat "/sudo:root@127.0.0.1:"
                                   buffer-file-name))))
;; }}

(setq imenu-max-item-length 256)

(use-package string-edit
  :straight t
  :commands (string-edit-at-point)
  :init
  (evil-leader/set-key "es" 'string-edit-at-point))


;; {{ eacl and other general grep (rgrep, grep ...) setup
(eval-after-load 'grep
  '(progn
     (dolist (v '("auto"
                  "target"
                  "node_modules"
                  "bower_components"
                  "*dist"
                  ".sass_cache"
                  ".cache"
                  ".npm"
                  "elpa"))
       (add-to-list 'grep-find-ignored-directories v))

     (dolist (v '("*.min.js"
                  "*.map"
                  "*.bundle.js"
                  "*.min.css"
                  "tags"
                  "TAGS"
                  "GTAGS"
                  "GRTAGS"
                  "GPATH"
                  "cscope.files"
                  "*.json"
                  "*.log"))
       (add-to-list 'grep-find-ignored-files v))))
;; }}

(defun optimize-emacs-startup ()
  "Speedup emacs startup by compiling."
  (interactive)
  (let* ((dir (file-truename "~/.emacs.d/modules/"))
         (files (directory-files dir)))
    ;; (load (file-truename "~/.emacs.d/init.el"))
    (dolist (f files)
      (when (string-match-p ".*\.el$" f)
        (let* ((default-directory dir))
          (byte-compile-file (file-truename f) t))))))

;; (use-package carbon-now-sh
;;   :straight t
;;   :commands (carbon-now-sh))

;; idle garbage collection
(defvar garbage-collection-timer nil
  "Timer that you can cancel, performs garbage collection on idle.")

(unless garbage-collection-timer
  (setq garbage-collection-timer
        (run-with-idle-timer 60 t 'garbage-collect)))

(global-set-key "\C-x\C-b" 'ibuffer)

(use-package ibuffer-vc
  :straight t
  :after ibuffer
  :commands (ibuffer-vc-set-filter-groups-by-vc-root)
  :hook (ibuffer-mode . ibuffer-vc-set-filter-groups-by-vc-root)
  :init (define-key ibuffer-mode-map "K" 'ibuffer-kill-filter-group))

(global-set-key (kbd "C-x \\") #'align-regexp)
(setq tab-always-indent 'complete)

;; nice scroll
;; (setq scroll-margin 0
;;       scroll-conservatively 100000
;;       scroll-preserve-screen-position 1)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; auto save buffers when they lost focus
;; (use-package super-save
;;   :config
;; (super-save-mode +1))

(use-package async
  :straight t
  :init
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1))

(use-package smtpmail-async
  :commands (async-smtpmail-send-it)
  :init
  (setq message-send-mail-function 'async-smtpmail-send-it)
  (setq send-mail-function 'async-smtpmail-send-it))

(use-package auth-source
  :no-require t
  :config (setq auth-sources '("~/.authinfo.gpg" "~/.netrc")))

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
(global-set-key "\C-x\C-l" 'narrow-or-widen-dwim)