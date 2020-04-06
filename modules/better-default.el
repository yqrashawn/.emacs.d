;;; -*- lexical-binding: t; -*-
(setq *is-a-mac* (eq system-type 'darwin))
(setq *win64* (eq system-type 'windows-nt))
(setq *cygwin* (eq system-type 'cygwin))
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)))
(setq *emacs24* (and (not (featurep 'xemacs)) (or (>= emacs-major-version 24))))
(setq *emacs25* (and (not (featurep 'xemacs)) (or (>= emacs-major-version 25))))
(setq *imac* (file-exists-p "~/yq.machine.home-imac"))
(setq *no-memory* (cond
                   (*is-a-mac*
                    (< (string-to-number (nth 1 (split-string (shell-command-to-string "sysctl hw.physmem")))) 2147483648))
                   (*linux* nil)
                   (t nil)))

(customize-set-variable 'inhibit-startup-screen t)
(customize-set-variable 'inhibit-startup-message t)
(customize-set-variable 'inhibit-startup-echo-area-message t)
(setq kmacro-ring-max 30)
(setq save-silently t)

;; enter pgp password in emacs
(setq epa-pinentry-mode 'loopback)
(setq epa-file-encrypt-to '("namy.19@gmail.com"))

;; https://emacs.stackexchange.com/questions/3673/how-to-make-vc-and-magit-treat-a-symbolic-link-to-a-real-file-in-git-repo-just
(setq find-file-visit-truename t)

(defvar dotspacemacs-auto-save-file-location 'nil
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
(auto-compression-mode t)

(setq read-process-output-max (* 1024 1024))

(use-feature emacs
  :init
  (setq-default
   echo-keystrokes 1e-6 ;; echo keystrokes quicker
   ns-use-native-fullscreen nil
   delete-by-moving-to-trash t
   create-lockfiles nil ;; no .# file
   disabled-command-function nil
   ad-redefinition-action 'accept
   custom-safe-themes t ;; treat all theme safe
   initial-scratch-message nil
   enable-recursive-minibuffers t  ;; Allow commands in minibuffers
   kill-buffer-query-functions nil ;; kill process buffer quicker
   ns-pop-up-frames nil)
  :config
  (setq-default indent-tabs-mode nil
                tab-width 2
                standard-indent 2
                c-basic-offset 2
                bidi-display-reordering nil)
  (defalias 'yes-or-no-p #'y-or-n-p))

(use-package dash
  :straight t
  :config (dash-enable-font-lock))

(use-package s
  :bind
  ("s-;" . transform-symbol-at-point)
  :config
  (defun transform-symbol-at-point ()
    (interactive)
    (put 'quit 'error-message "")
    (let* ((choices '((?c . s-lower-camel-case)
                      (?C . s-upper-camel-case)
                      (?_ . s-snake-case)
                      (?- . s-dashed-words)
                      (?d . s-downcase)
                      (?u . s-upcase)))
           (chars (mapcar #'car choices))
           (prompt (concat "Transform symbol at point [" chars "]: "))
           (escape-chars '(?\s ?\d ?\t ?\b ?\e ?\r))
           (ch (read-char-choice prompt (append chars escape-chars)))
           (fn (assoc-default ch choices))
           (symbol (thing-at-point 'symbol t))
           (bounds (bounds-of-thing-at-point 'symbol)))
      (when fn
        (delete-region (car bounds) (cdr bounds))
        (insert (funcall fn symbol))
        (when (looking-at " ") (forward-char)))
      (keyboard-quit)
      (run-at-time nil nil (add-lam (put 'quit 'error-message "Quit"))))))

(use-package alert
  :straight t
  :defer t
  :custom
  (alert-default-style 'osx-notifier)
  :config
  (defun alert-after-finish-in-background (buf str)
    (unless (get-buffer-window buf 'visible)
      (alert str :buffer buf))))

(use-feature profiler
  :defer t
  :bind
  ("C-x p r"  . profiler-report)
  ("C-x p 1"  . profiler-start)
  ("C-x p 0"  . profiler-stop))

(use-feature warnings
  :custom
  (warning-suppress-types '((undo discard-info))))

(use-feature comint
  :defer t
  :custom
  (comint-prompt-read-only t)
  (comint-input-ignoredups t)
  (comint-scroll-show-maximum-output nil)
  :init
  (setq-default
   comint-output-filter-functions
   '(ansi-color-process-output
     comint-truncate-buffer
     comint-watch-for-password-prompt))
  (defun text-smaller-no-truncation ()
    (set (make-local-variable 'scroll-margin) 0)
    (text-scale-set -0.25))
  (defun comint-return-dwim ()
    (interactive)
    (cond
     ((comint-after-pmark-p)
      (comint-send-input))
     ((ffap-url-at-point)
      (browse-url (ffap-url-at-point)))
     ((ffap-file-at-point)
      (find-file (ffap-file-at-point)))
     (t
      (comint-next-prompt 1))))
  :hook
  (comint-mode . text-smaller-no-truncation)
  :bind
  (:map comint-mode-map
        ("RET"       . comint-return-dwim)
        ("C-r"       . comint-history-isearch-backward-regexp)
        ("C-u"       . comint-clear-buffer)
        ("C-p"     . comint-previous-matching-input-from-input)
        ("C-n" . comint-next-matching-input-from-input))
  :config
  (define-key comint-mode-map (kbd "C-d") nil)
  (add-hook 'kill-buffer-hook #'comint-write-input-ring)
  (add-lam 'kill-emacs-hook
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer (comint-write-input-ring)))))

(use-feature compile
  :defer t
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  :hook
  (compilation-mode . text-smaller-no-truncation)
  :config
  (add-hook 'compilation-finish-functions #'alert-after-finish-in-background))

(use-feature shell
  :defer t
  :init
  ;; make emacs recognize shell command alias
  (setq shell-command-switch "-ic")
  (setq shell-file-name "zsh")
  :config
  (define-key shell-mode-map (kbd "s-l") #'spacemacs/alternate-buffer)
  (defun make-shell-command-behave-interactively (orig-fun &rest args)
    (let ((shell-command-switch "-ic"))
      (apply orig-fun args)))
  (advice-add 'shell-command :around #'make-shell-command-behave-interactively)
  (advice-add 'start-process-shell-command :around #'make-shell-command-behave-interactively))

(use-feature executable
  :defer t
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p))

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq-default frame-title-format
              '(:eval
                (format "%s@%s: %s"
                        (or (file-remote-p default-directory 'user)
                            user-real-login-name)
                        (or (file-remote-p default-directory 'host)
                            system-name)
                        (cond
                         (buffer-file-truename buffer-file-truename)
                         (dired-directory dired-directory)
                         (t "%b")))))

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(add-hook 'prog-mode-hook 'goto-address-prog-mode)

;; https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/bug-reference.el
;; not using this right now, maybe add bug url format for jira
;; (add-hook 'prog-mode-hook 'bug-reference-prog-mode)

;; prettify symbol eg. lambda to λ
;; (global-prettify-symbols-mode +1)

(setq help-window-select 't)
(setq compilation-scroll-output 'first-error)
(setq ffap-machine-p-known 'reject)
(setq dired-dwim-target t)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)
;; (add-to-list 'completion-styles 'flex t)
;; (setq completion-styles '(basic partial-completion emacs22))
(xterm-mouse-mode 1)
(setq initial-major-mode 'text-mode)
(setq-default fill-column 80)
(use-feature abbrev
  :diminish abbrev-mode
  :custom
  (abbrev-file-name (concat user-emacs-directory "abbrev_defs"))
  :init
  (abbrev-mode 1)
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

(setq-default sentence-end-double-space nil)
(setq window-combination-resize t)
(setq column-number-mode t)
(blink-cursor-mode -1)
(setq x-underline-at-descent-line t)

;; don't let the cursor go into minibuffer prompt
;; Tip taken from Xah Lee: http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(customize-set-variable
 'minibuffer-prompt-properties
 '(read-only t cursor-intangible t face minibuffer-prompt))


(fset 'yes-or-no-p 'y-or-n-p)



(use-feature files
  :custom
  (make-backup-files nil)
  (confirm-kill-processes nil)
  (confirm-kill-emacs nil)
  (enable-local-variables t)
  (remote-file-name-inhibit-cache 1800)
  (confirm-nonexistent-file-or-buffer nil)
  :init
  (defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))
  (spacemacs/set-leader-keys "fU" 'revert-buffer-no-confirm)

  ;; Auto-save file
  (setq auto-save-default (not (null dotspacemacs-auto-save-file-location)))
  (setq auto-save-list-file-prefix (concat spacemacs-auto-save-directory))

  ;; always save TRAMP URLs to cache directory no matter what is the value
  ;; of `dotspacemacs-auto-save-file-location'
  (let ((autosave-dir (concat spacemacs-auto-save-directory "dist/")))
    (setq auto-save-file-name-transforms
          `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,autosave-dir  t)))
    (unless (or (file-exists-p autosave-dir)
                (null dotspacemacs-auto-save-file-location))
      (make-directory autosave-dir t)))

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
  :config
  (defun find-file-maybe-make-directories ()
    (let ((dir (file-name-directory buffer-file-name)))
      (unless (file-exists-p dir)
        (make-directory dir t))))
  (push #'find-file-maybe-make-directories find-file-not-found-functions))

(use-feature tramp
  :defer t
  :custom
  (tramp-default-method "scp")
  (vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp))
  (tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
  (tramp-connection-timeout 15)
  (tramp-completion-reread-directory-timeout 900)
  :config
  (add-to-list 'tramp-connection-properties
               (list ".*" "locale" "LC_ALL=C"))
  ;; access file inside docker even on remote machine
  ;; https://willschenk.com/articles/2020/tramp_tricks/
  (push
   (cons
    "docker"
    '((tramp-login-program "docker")
      (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
      (tramp-remote-shell "/bin/sh")
      (tramp-remote-shell-args ("-i") ("-c"))))
   tramp-methods)

  (defadvice tramp-completion-handle-file-name-all-completions
      (around dotemacs-completion-docker activate)
    "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\" returns
    a list of active Docker container names, followed by colons."
    (if (equal (ad-get-arg 1) "/docker:")
        (let* ((dockernames-raw (shell-command-to-string "docker ps | awk '$NF != \"NAMES\" { print $NF \":\" }'"))
               (dockernames (cl-remove-if-not
                             #'(lambda (dockerline) (string-match ":$" dockerline))
                             (split-string dockernames-raw "\n"))))
          (setq ad-return-value dockernames))
      ad-do-it)))

(use-feature simple
  :custom
  (save-interprogram-paste-before-kill t)
  (next-error-recenter t)
  (async-shell-command-buffer 'new-buffer)
  :bind
  ;; ("C-`" . list-processes)
  ([remap goto-line] . goto-line-with-linum-mode)
  (:map minibuffer-local-map
        ("<escape>"  . abort-recursive-edit))
  :hook
  ((prog-mode text-mode) . turn-on-auto-fill)
  :config
  ;; https://with-emacs.com/posts/editing/show-matching-lines-when-parentheses-go-off-screen/
  ;; we will call `blink-matching-open` ourselves...
  (remove-hook 'post-self-insert-hook
               #'blink-paren-post-self-insert-function)

  ;; this still needs to be set for `blink-matching-open` to work
  (setq blink-matching-paren 'show)
  (defun display-line-overlay+ (pos str &optional face)
    "Display line at POS as STR with FACE.

FACE defaults to inheriting from default and highlight."
    (let ((ol (save-excursion
                (goto-char pos)
                (make-overlay (line-beginning-position)
                              (line-end-position)))))
      (overlay-put ol 'display str)
      (overlay-put ol 'face
                   (or face '(:inherit default :inherit highlight)))
      ol))
  (let ((+show-paren-overlay nil)) ; keep track of the overlay
    (advice-add
     #'show-paren-function
     :after
     (defun show-paren--off-screen+ (&rest _args)
       "Display matching line for off-screen paren."
       (when (overlayp +show-paren-overlay)
         (delete-overlay +show-paren-overlay))
       ;; check if it's appropriate to show match info,
       ;; see `blink-paren-post-self-insert-function'
       (when (and (overlay-buffer show-paren--overlay)
                  (not (or cursor-in-echo-area
                           executing-kbd-macro
                           noninteractive
                           (minibufferp)
                           this-command))
                  (and (not (bobp))
                       (memq (char-syntax (char-before)) '(?\) ?\$)))
                  (= 1 (logand 1 (- (point)
                                    (save-excursion
                                      (forward-char -1)
                                      (skip-syntax-backward "/\\")
                                      (point))))))
         ;; rebind `minibuffer-message' called by
         ;; `blink-matching-open' to handle the overlay display
         (cl-letf (((symbol-function #'minibuffer-message)
                    (lambda (msg &rest args)
                      (let ((msg (apply #'format-message msg args)))
                        (setq +show-paren-overlay (display-line-overlay+
                                                   (window-start) msg))))))
           (blink-matching-open))))))
  (setq show-paren-style 'paren
        show-paren-delay 0.03
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren nil
        show-paren-when-point-in-periphery t)

  (defun goto-line-with-linum-mode ()
    (interactive)
    (let ((linum-not-enabled (eq nil linum-mode)))
      (linum-mode 1)
      (unwind-protect
          (call-interactively #'goto-line)
        (when linum-not-enabled
          (linum-mode -1)))))
  (defun pop-to-process-list-buffer ()
    (pop-to-buffer "*Process List*"))
  (advice-add 'list-processes :after #'pop-to-process-list-buffer))

;; remove annoying ellipsis when printing sexp in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

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

(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'org-mode-hook #'hs-minor-mode)


;; C-h key as BS
(keyboard-translate ?\C-h ?\C-?)
(global-set-key [(control ?h)] 'delete-backward-char)

(defun yq/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member
           major-mode
           spacemacs-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (indent-region
           (region-beginning)
           (region-end))
        (if (and (fboundp 'lispyvile-prettify) (memq major-mode '(clojure-mode emacs-lisp-mode)))
            (lispyville-prettify
             (point-min)
             (point-max))
          (evil-indent
           (point-min)
           (point-max))))
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

(defun yq/bury-this-buffer (&optional arg)
  "Kill the current buffer.
If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal '(4) arg)
        (progn
          (bury-buffer)
          (yq/delete-window))
      (bury-buffer))))

(global-set-key (kbd "s-k") 'yq/kill-this-buffer)
(global-set-key (kbd "C-x k") 'yq/kill-this-buffer)

(defun yq/delete-window (&optional arg)
  "Delete the current window.
If the universal prefix argument is used then kill the buffer too."
  (interactive "P")
  (if (equal '(4) arg)
      (kill-buffer-and-window)
    (delete-window)))

(get-buffer-window "*Help*")
(use-package mwim
  :straight t
  :commands (mwim-beginning-of-code-or-line mwim-end-of-code-or-line)
  :init
  (global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (global-set-key (kbd "C-e") 'mwim-end-of-code-or-line))

(add-hook 'hs-minor-mode-hook (lambda () (diminish 'hs-minor-mode)))
(add-hook 'auto-revert-mode-hook (lambda () (diminish 'auto-revert-mode)))

(require 'electric)
(require 'elec-pair)
(electric-indent-mode 1)
;; (electric-layout-mode 1)
(electric-quote-mode 1)
(add-to-list 'electric-pair-pairs '(?{ . ?}))
(add-to-list 'electric-pair-pairs '(?( . ?)))
(add-to-list 'electric-pair-pairs '(?[ . ?]))
(add-to-list 'electric-pair-pairs '(?` . ?`))
(add-to-list 'electric-pair-pairs '(?` . ?'))
(add-to-list 'electric-quote-chars ?`)

(yq/add-toggle visual-line :mode visual-line-mode)
(setq org-confirm-babel-evaluate nil)
(setq vc-follow-symlinks nil)
(yq/add-toggle show-paren :mode show-paren-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)

(use-package windmove
  :defer t
  :bind
  (("C-M-s-7 h" . #'windmove-left)
   ("C-M-s-7 l" . #'windmove-right)
   ("C-M-s-7 j" . #'windmove-down)
   ("C-M-s-7 k" . #'windmove-up))
  :config
  (defun hydra-move-splitter-left (arg)
    "Move window splitter left."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun hydra-move-splitter-right (arg)
    "Move window splitter right."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

  (defun hydra-move-splitter-up (arg)
    "Move window splitter up."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (enlarge-window arg)
      (shrink-window arg)))

  (defun hydra-move-splitter-down (arg)
    "Move window splitter down."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (shrink-window arg)
      (enlarge-window arg)))

  (global-set-key (kbd "C-x 8 w h")  'hydra-move-splitter-left)
  (global-set-key (kbd "C-x 8 w l")  'hydra-move-splitter-right)
  (global-set-key (kbd "C-x 8 w j")  'hydra-move-splitter-down)
  (global-set-key (kbd "C-x 8 w k")  'hydra-move-splitter-up)

  (defun yq/split-window-right ()
    (interactive)
    (split-window-right)
    (windmove-right))

  (defun yq/split-window-below ()
    (interactive)
    (split-window-below)
    (windmove-down))

  (global-set-key (kbd "C-x 3") 'yq/split-window-right)
  (global-set-key (kbd "C-x 2") 'yq/split-window-below))

(use-package buffer-move
  :straight t
  :commands (buf-move-right buf-move-left buf-move-down buf-move-up)
  :bind (("C-x 9 w h" . 'buf-move-left)
         ("C-x 9 w l" . 'buf-move-right)
         ("C-x 9 w j" . 'buf-move-down)
         ("C-x 9 w k" . 'buf-move-up)))

(use-package winner
  :bind(("C-M-s-7 u". 'winner-undo)
        ("C-M-s-7 r". 'winner-redo))
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
                                          "*esh command on file*"))

  (setq winner-boring-buffers
        (append winner-boring-buffers spacemacs/winner-boring-buffers))
  (winner-mode t))


(with-eval-after-load 'hi-lock
  (diminish 'hi-lock-mode))

(yq/add-toggle line-numbers :mode display-line-numbers-mode)
(spacemacs/set-leader-keys "tn" 'yq/toggle-line-numbers)

(use-feature savehist
  :init
  ;; Minibuffer history
  (setq savehist-file (concat spacemacs-cache-directory "savehist")
        history-length 1024
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
        savehist-autosave-interval 60)
  (savehist-mode t))

;; recentf
(use-feature recentf
  :init
  (setq recentf-keep '(file-remote-p file-readable-p))
  (setq recentf-save-file (concat user-emacs-directory "recentf")
        recentf-max-saved-items 100
        recentf-auto-cleanup 'never
        recentf-auto-save-timer (run-with-idle-timer 300 t
                                                     'recentf-save-list))
  (add-hook 'delete-terminal-functions 'recentf-save-list)
  (recentf-mode 1)
  :config
  (setq recentf-max-saved-items 1000)
  (with-eval-after-load 'recentf
    (run-at-time nil (* 5 60) 'recentf-save-list)
    ;; (add-to-list 'recentf-exclude
    ;;              (file-truename spacemacs-cache-directory))
    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
    (add-to-list 'recentf-exclude "/private/var/folders/")
    (add-to-list 'recentf-exclude "/usr/local/Cellar/emacs")
    ;; (add-to-list 'recentf-exclude (concat user-home-directory "Dropbox/ORG"))
    ;; (add-to-list 'recentf-exclude (concat user-home-directory "Dropbox/Books"))
    (add-to-list 'recentf-exclude (expand-file-name (concat user-emacs-directory "straight/build")))
    (add-to-list 'recentf-exclude (expand-file-name (concat user-emacs-directory "persp-confs/")))
    (add-to-list 'recentf-exclude "/var/folders/")
    (add-to-list 'recentf-exclude "/var/tmp/")
    (add-to-list 'recentf-exclude (expand-file-name (concat user-emacs-directory "recentf")))
    (add-to-list 'recentf-exclude "/tmp/")
    (add-to-list 'recentf-exclude "\\indium-eval-.*")))

;; saveplace remembers your location in a file when saving files
(use-feature saveplace
  :init
  (if (fboundp 'save-place-mode)
      ;; Emacs 25 has a proper mode for `save-place'
      (save-place-mode)
    (setq save-place t))
  ;; Save point position between sessions
  (setq save-place-file (concat spacemacs-cache-directory "places")))

(use-feature whitespace
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
  :custom
  (bookmark-default-file (concat spacemacs-cache-directory "bookmarks"))
  (bookmark-save-flag 1))

(use-package popwin
  :straight (:host github :repo "yqrashawn/popwin-el")
  :init
  (setq popwin:reuse-window 'visible)
  :config
  (popwin-mode 1)
  (spacemacs/set-leader-keys "bm" 'popwin:messages)
  (spacemacs/set-leader-keys "bc" 'popwin:close-popup-window)
  (setq
   popwin:special-display-config
   '(("*cider-error*"                       :dedicated t   :position bottom :stick t    :noselect t   :height 0.4)
     ("*PLANTUML Preview*"                  :dedicated t   :position bottom :stick t    :noselect t   :height 0.4)
     ("\*rg-scan-async\*.*"      :regexp t  :dedicated t   :position bottom :stick nil  :noselect t   :height 0.1)
     ("*Contents*"                          :dedicated t   :position bottom :stick t    :noselect t   :height 0.4)
     ;; ("*Occur*"                             :dedicated nil :position bottom :stick t    :noselect nil :height 0.4)
     ("\*helpful\ .*\*"          :regexp t  :dedicated t   :position right  :stick t    :noselect nil :width 0.5 :kill t)
     ("*Help*"                              :dedicated t   :position bottom :stick t    :noselect t   :height 0.5 :kill t)
     ("*cider-doc*"                         :dedicated t   :position bottom :stick t    :noselect t   :height 0.4)
     ("*Backtrace*"                         :dedicated t   :position bottom :stick t    :noselect t   :height 0.4 :kill t)
     ("*Warnings*"                          :dedicated t   :position bottom :stick t    :noselect t   :height 0.4 :kill t)
     ("*Messages*"                          :dedicated t   :position left   :stick t    :noselect t   :width  0.5)
     ("*Org-Babel Error Output*"            :dedicated t   :position bottom :stick nil  :noselect t   :width  0.5)
     ("*Org Babel Results*"                 :dedicated t   :position bottom :stick t    :noselect t   :width  0.5)
     ("*Org-Babel Results*"                 :dedicated t   :position bottom :stick t    :noselect t   :width  0.5)
     ("*mu4e-update*"                       :dedicated t   :position bottom :stick nil  :noselect t)
     ("*HTTP Response*"                     :dedicated t   :position bottom :stick t    :noselect t   :height 0.4 :kill t)
     ("*compilation*"                       :dedicated t   :position bottom :stick t    :noselect t   :height 0.4 :kill t)
     ("*Shell Command Output*"              :dedicated t   :position bottom :stick t    :noselect t   :height 0.4 :kill t)
     ("*prettier errors*"                   :dedicated t   :position bottom :stick t    :noselect t   :height 0.4 :kill t)
     ("*Async Shell Command*"               :dedicated t   :position bottom :stick t    :noselect t   :kill t)
     ("*undo-tree*"                         :dedicated t   :position right  :stick t    :noselect nil :width 60)
     ("*undo-tree Diff*"                    :dedicated t   :position bottom :stick t    :noselect nil :height 0.3 :kill t)
     ("*ert*"                               :dedicated t   :position bottom :stick t    :noselect t :kill t)
     ("*grep*"                              :dedicated t   :position bottom :stick t    :noselect nil :kill t)
     ("*nosetests*"                         :dedicated t   :position bottom :stick t    :noselect nil :kill t)
     ("^\*WoMan.+\*$"           :regexp t   :dedicated t   :position bottom             :noselect t)
     ("^\*projector:\ .*\*$"    :regexp t   :dedicated t   :position right  :stick t    :noselect t :width .4 :kill t))))

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

(defun narrow-to-region-indirect-buffer (start end)
  (interactive "r")
  (with-current-buffer (clone-indirect-buffer
                        (generate-new-buffer-name
                         (concat (buffer-name) "-indirect-"
                                 (number-to-string start) "-"
                                 (number-to-string end)))
                        'display)
    (narrow-to-region start end)
    (deactivate-mark)
    (goto-char (point-min))))

;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))
(global-set-key "\C-xn" 'narrow-or-widen-dwim)
(global-set-key "\C-x\C-n" 'narrow-or-widen-dwim)

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

(add-hook 'prog-mode-hook (defl (setq-local comment-auto-fill-only-comments t)))
(yq/add-toggle auto-fill :mode auto-fill-mode)
(spacemacs/set-leader-keys "tF" 'yq/toggle-auto-fill)

;; (use-package edit-server
;;   :defer t
;;   :straight t
;;   :config (edit-server-start))
;; (use-package atomic-chrome
;;   :defer t
;;   :straight t
;;   :config (atomic-chrome-start-server))

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

(spacemacs/set-leader-keys "rb" 'spacemacs/rename-current-buffer-file)

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

(spacemacs/set-leader-keys "fd" 'spacemacs/delete-file)

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

(defun spacemacs/sudo-edit (&optional arg)
  (interactive "P")
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "File: ")
                 buffer-file-name)))
    (find-file
     (cond ((string-match-p "^/ssh:" fname)
            (with-temp-buffer
              (insert fname)
              (search-backward ":")
              (let ((last-match-end nil)
                    (last-ssh-hostname nil))
                (while (string-match "@\\\([^:|]+\\\)" fname last-match-end)
                  (setq last-ssh-hostname (or (match-string 1 fname)
                                              last-ssh-hostname))
                  (setq last-match-end (match-end 0)))
                (insert (format "|sudo:%s" (or last-ssh-hostname "localhost"))))
              (buffer-string)))
           (t (concat "/sudo:root@localhost:" fname))))))

(defun yq/fix-evil-state-bug ()
  ;; https://github.com/emacs-evil/evil/issues/301
  (evil-insert-state)
  (evil-normal-state))

(use-package ssh-config-mode
  :straight t
  :mode ("~/.ssh/config". ssh-config-mode))

(use-package gitconfig-mode
  :straight t
  :mode (("\\.gitconfig\\'" . gitconfig-mode)
         ("\\.git/config\\'" . gitconfig-mode)
         ("\\.gitmodules\\'" . gitconfig-mode)))

(use-package gitignore-mode
  :straight t
  :mode ("\\.gitignore\\'" . gitignore-mode))

(use-package gitattributes-mode
  :straight t
  :mode "/\\.gitattributes\\'" "/\\.git/info/attributes\\'" "/git/attributes\\'")

(use-feature autoinsert
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

;; font scale with command key
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

(use-feature info
  :defer t
  :init
  (spacemacs/set-leader-keys "?" #'info-display-manual)
  :config
  (define-key Info-mode-map "s" nil)
  (define-key Info-mode-map "ss" #'Info-search)
  (define-key Info-mode-map "sj" #'counsel-recentf)
  (define-key Info-mode-map (kbd "s SPC") #'counsel-M-x)
  (define-key Info-mode-map "sc" 'yq/delete-window)
  (define-key Info-mode-map "sk" 'yq/kill-this-buffer)
  (evil-define-key 'normal
    "s" nil
    "sj" 'counsel-recentf
    (kbd "s SPC") 'counsel-M-x))

(spacemacs/set-leader-keys "xdw" 'delete-trailing-whitespace)

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
(define-key evil-normal-state-map (kbd "<tab>") 'switch-to-most-recent-buffer)

;;fast switching between three buffers
(define-key evil-normal-state-map (kbd "<C-tab>") 'switch-to-second-most-recent-buffer)
(define-key evil-normal-state-map (kbd "<C-s-tab>") 'switch-to-third-most-recent-buffer)

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

(setq history-delete-duplicates t)

(setq imenu-max-item-length 1024)

(use-package string-edit
  :straight t
  :defer t
  :init
  (evil-leader/set-key "es" #'string-edit-at-point))


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

;; not used right know
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

;; idle garbage collection
(defvar garbage-collection-timer nil
  "Timer that you can cancel, performs garbage collection on idle.")

(unless garbage-collection-timer
  (setq garbage-collection-timer
        (run-with-idle-timer 60 t 'garbage-collect)))

;; ibuffer
(use-package ibuffer-vc
  :straight t
  :after ibuffer
  :commands (ibuffer-vc-set-filter-groups-by-vc-root)
  :hook (ibuffer-mode . ibuffer-vc-set-filter-groups-by-vc-root)
  :init (define-key ibuffer-mode-map "K" 'ibuffer-kill-filter-group))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)))

(with-eval-after-load 'ibuffer
  (defun ibuffer-advance-motion (direction)
    (forward-line direction)
    (beginning-of-line)
    (if (not (get-text-property (point) 'ibuffer-filter-group-name))
        t
      (ibuffer-skip-properties '(ibuffer-filter-group-name)
                               direction)
      nil))
  (defun ibuffer-previous-line (&optional arg)
    "Move backwards ARG lines, wrapping around the list if necessary."
    (interactive "P")
    (or arg (setq arg 1))
    (let (err1 err2)
      (while (> arg 0)
        (cl-decf arg)
        (setq err1 (ibuffer-advance-motion -1)
              err2 (if (not (get-text-property (point) 'ibuffer-title))
                       t
                     (goto-char (point-max))
                     (beginning-of-line)
                     (ibuffer-skip-properties '(ibuffer-summary
                                                ibuffer-filter-group-name)
                                              -1)
                     nil)))
      (and err1 err2)))
  (defun ibuffer-next-line (&optional arg)
    "Move forward ARG lines, wrapping around the list if necessary."
    (interactive "P")
    (or arg (setq arg 1))
    (let (err1 err2)
      (while (> arg 0)
        (cl-decf arg)
        (setq err1 (ibuffer-advance-motion 1)
              err2 (if (not (get-text-property (point) 'ibuffer-summary))
                       t
                     (goto-char (point-min))
                     (beginning-of-line)
                     (ibuffer-skip-properties '(ibuffer-summary
                                                ibuffer-filter-group-name
                                                ibuffer-title)
                                              1)
                     nil)))
      (and err1 err2)))
  (setq mp/ibuffer-collapsed-groups (list "Default"))
  (defadvice ibuffer (after collapse-helm)
    (dolist (group mp/ibuffer-collapsed-groups)
      (progn
        (goto-char 1)
        (when (search-forward (concat "[ " group " ]") (point-max) t)
          (progn
            (move-beginning-of-line nil)
            (ibuffer-toggle-filter-group)))))
    (goto-char 1)
    (search-forward "[ " (point-max) t))
  (ad-activate 'ibuffer)

  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))

  ;; Modify the default ibuffer-formats
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)))

  (evilified-state-evilify ibuffer-mode ibuffer-mode-map
    (kbd "j") 'ibuffer-next-line
    (kbd "k") 'ibuffer-previous-line
    (kbd "sl") 'ibuffer-redisplay
    (kbd "v") 'ibuffer-visit-buffer
    (kbd "SPC") nil
    (kbd "SPC SPC") 'counsel-M-x
    (kbd "TAB") 'ibuffer-toggle-filter-group
    (kbd "f") 'ivy-switch-buffer
    (kbd "n") 'ibuffer-forward-filter-group
    (kbd "p") 'ibuffer-backward-filter-group)
  (defadvice
      ibuffer (around ibuffer-point-to-most-recent) ()
      "Open ibuffer with cursor pointed to most recent (non-minibuffer) buffer name"
      (let ((recent-buffer-name
             (if (minibufferp (buffer-name))
                 (buffer-name
                  (window-buffer (minibuffer-selected-window)))
               (buffer-name (other-buffer)))))
        ad-do-it
        (ibuffer-jump-to-buffer recent-buffer-name)))
  (ad-activate 'ibuffer))

(use-package hydra
  :straight t
  :defer t
  :init
  ;; ibuffer
  (defhydra hydra-ibuffer-main (:color pink :hint nil)
    "
  ^Mark^        | ^Actions^        | ^View^
  -^----^-------+-^-------^--------+-^----^-------
  _m_: mark     | _D_: delete      | _g_: refresh
  _u_: unmark   | _S_: save        | _s_: sort
  _*_: specific | _a_: all actions | _/_: filter
  ^-----------^-+-^-------^--------+-^-------^----
  "
    ;; ("j" ibuffer-forward-line)
    ("RET" ibuffer-visit-buffer :color blue)
    ;; ("k" ibuffer-backward-line)

    ("m" ibuffer-mark-forward)
    ("u" ibuffer-unmark-forward)
    ("*" hydra-ibuffer-mark/body :color blue)

    ("D" ibuffer-do-delete)
    ("S" ibuffer-do-save)
    ("a" hydra-ibuffer-action/body :color blue)

    ("g" ibuffer-update)
    ("s" hydra-ibuffer-sort/body :color blue)
    ("/" hydra-ibuffer-filter/body :color blue)

    ("o" ibuffer-visit-buffer-other-window "other window" :color blue)
    ("q" quit-window "quit ibuffer" :color blue)
    ("." nil "toggle hydra" :color blue))
  (defhydra hydra-ibuffer-mark (:color teal :columns 5
                                       :after-exit (hydra-ibuffer-main/body))
    "Mark"
    ("*" ibuffer-unmark-all "unmark all")
    ("M" ibuffer-mark-by-mode "mode")
    ("m" ibuffer-mark-modified-buffers "modified")
    ("u" ibuffer-mark-unsaved-buffers "unsaved")
    ("s" ibuffer-mark-special-buffers "special")
    ("r" ibuffer-mark-read-only-buffers "read-only")
    ("/" ibuffer-mark-dired-buffers "dired")
    ("e" ibuffer-mark-dissociated-buffers "dissociated")
    ("h" ibuffer-mark-help-buffers "help")
    ("z" ibuffer-mark-compressed-file-buffers "compressed")
    ("b" hydra-ibuffer-main/body "back" :color blue))
  (defhydra hydra-ibuffer-action (:color teal :columns 4
                                         :after-exit
                                         (if (eq major-mode 'ibuffer-mode)
                                             (hydra-ibuffer-main/body)))
    "Action"
    ("A" ibuffer-do-view "view")
    ("E" ibuffer-do-eval "eval")
    ("F" ibuffer-do-shell-command-file "shell-command-file")
    ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
    ("H" ibuffer-do-view-other-frame "view-other-frame")
    ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
    ("M" ibuffer-do-toggle-modified "toggle-modified")
    ("O" ibuffer-do-occur "occur")
    ("P" ibuffer-do-print "print")
    ("Q" ibuffer-do-query-replace "query-replace")
    ("R" ibuffer-do-rename-uniquely "rename-uniquely")
    ("T" ibuffer-do-toggle-read-only "toggle-read-only")
    ("U" ibuffer-do-replace-regexp "replace-regexp")
    ("V" ibuffer-do-revert "revert")
    ("W" ibuffer-do-view-and-eval "view-and-eval")
    ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
    ("b" nil "back"))
  (defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
    "Sort"
    ("i" ibuffer-invert-sorting "invert")
    ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
    ("v" ibuffer-do-sort-by-recency "recently used")
    ("s" ibuffer-do-sort-by-size "size")
    ("f" ibuffer-do-sort-by-filename/process "filename")
    ("m" ibuffer-do-sort-by-major-mode "mode")
    ("b" hydra-ibuffer-main/body "back" :color blue))
  (defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
    "Filter"
    ("m" ibuffer-filter-by-used-mode "mode")
    ("M" ibuffer-filter-by-derived-mode "derived mode")
    ("n" ibuffer-filter-by-name "name")
    ("c" ibuffer-filter-by-content "content")
    ("e" ibuffer-filter-by-predicate "predicate")
    ("f" ibuffer-filter-by-filename "filename")
    (">" ibuffer-filter-by-size-gt "size")
    ("<" ibuffer-filter-by-size-lt "size")
    ("/" ibuffer-filter-disable "disable")
    ("b" hydra-ibuffer-main/body "back" :color blue))
  (with-eval-after-load 'ibuffer
    (define-key ibuffer-mode-map "." 'hydra-ibuffer-main/body)
    (add-hook 'ibuffer-hook #'hydra-ibuffer-main/body))

  ;; Info mode
  (defhydra hydra-info (:color blue :hint nil)
    "
Info-mode:

  ^^_]_ forward  (next logical node)       ^^_l_ast (←)        _u_p (↑)                             _f_ollow reference       _T_OC
  ^^_[_ backward (prev logical node)       ^^_r_eturn (→)      _m_enu (↓) (C-u for new window)      _i_ndex                  _d_irectory
  ^^_n_ext (same level only)               ^^_H_istory         _g_oto (C-u for new window)          _,_ next index item      _c_opy node name
  ^^_p_rev (same level only)               _<_/_t_op           _b_eginning of buffer                virtual _I_ndex          _C_lone buffer
  regex _s_earch (_S_ case sensitive)      ^^_>_ final         _e_nd of buffer                      ^^                       _a_propos

  _1_ .. _9_ Pick first .. ninth item in the node's menu.

"
    ("]" Info-forward-node)
    ("[" Info-backward-node)
    ("n" Info-next)
    ("p" Info-prev)
    ("s" Info-search)
    ("S" Info-search-case-sensitively)

    ("h" evil-backward-char)
    ("j" evil-next-line)
    ("k" evil-previous-line)
    ("l" evil-forward-char)
    ("L" Info-history-back)
    ("r" Info-history-forward)
    ("H" Info-history)
    ("t" Info-top-node)
    ("<" Info-top-node)
    (">" Info-final-node)

    ("u" Info-up)
    ("^" Info-up)
    ("m" Info-menu)
    ("g" Info-goto-node)
    ("b" beginning-of-buffer)
    ("e" end-of-buffer)

    ("f" Info-follow-reference)
    ("i" Info-index)
    ("," Info-index-next)
    ("I" Info-virtual-index)

    ("T" Info-toc)
    ("d" Info-directory)
    ("c" Info-copy-current-node-name)
    ("C" clone-buffer)
    ("a" info-apropos)

    ("1" Info-nth-menu-item)
    ("2" Info-nth-menu-item)
    ("3" Info-nth-menu-item)
    ("4" Info-nth-menu-item)
    ("5" Info-nth-menu-item)
    ("6" Info-nth-menu-item)
    ("7" Info-nth-menu-item)
    ("8" Info-nth-menu-item)
    ("9" Info-nth-menu-item)

    ("?" Info-summary "Info summary")
    ("h" Info-help "Info help")
    ("q" Info-exit "Info exit")
    ("." nil "toggle hydra" :color blue)
    ("C-g" nil "cancel" :color blue))

  ;; (add-hook 'Info-mode-hook #'hydra-info/body)
  (define-key Info-mode-map "." 'hydra-info/body))

(global-set-key (kbd "C-x \\") #'align-regexp)
;; (setq tab-always-indent 'complete)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package async
  :straight t
  :init
  ;; (autoload 'dired-async-mode "dired-async.el" nil t)
  ;; (dired-async-mode 1)
  (async-bytecomp-package-mode 1))

(use-package smtpmail-async
  :commands (async-smtpmail-send-it)
  :init
  (setq send-mail-function 'async-smtpmail-send-it
        message-send-mail-function 'async-smtpmail-send-it))

(use-feature auth-source
  :defer t
  :custom
  (auth-sources '("~/.authinfo.gpg" "~/.netrc"))
  (auth-source-gpg-encrypt-to '("namy.19@gmail.com"))
  :no-require t)

;; (use-feature ansi-color
;;   :defer t
;;   :config
;;   (add-to-list
;;    'eshell-output-filter-functions
;;    (defl
;;        (ansi-color-apply-on-region
;;         eshell-last-output-start
;;         eshell-last-output-end))))

;; like golden ratio mode
;; (use-package zoom
;;   :straight t
;;   :init
;;   (defun size-callback ()
;;     (cond ((> (frame-pixel-width) 1280) '(90 . 0.75))
;;           (t                            '(0.5 . 0.5))))
;;   (setq zoom-size 'size-callback)
;;   (setq zoom-ignored-major-modes '(term-mode))
;;   (yq/add-toggle zoom :mode zoom-mode)
;;   (evil-leader/set-key "tz" 'yq/toggle-zoom))

(add-hook 'makefile-mode-hook 'whitespace-mode)

(defvar load-user-customized-major-mode-hook nil)
(defun is-buffer-file-temp ()
  (interactive)
  "If (buffer-file-name) is nil or a temp file or HTML file converted from org file"
  (let ((f (buffer-file-name))
        org
        (rlt t))
    (cond
     ((not load-user-customized-major-mode-hook) t)
     ((not f)
      ;; file does not exist at all
      (setq rlt t))
     ((string= f cached-normal-file-full-path)
      (setq rlt nil))
     ((string-match (concat "^" temporary-file-directory) f)
      ;; file is create from temp directory
      (setq rlt t))
     ((and (string-match "\.html$" f)
           (file-exists-p
            (setq org (replace-regexp-in-string "\.html$" ".org" f))))
      ;; file is a html file exported from org-mode
      (setq rlt t))
     (force-buffer-file-temp-p
      (setq rlt t))
     (t
      (setq cached-normal-file-full-path f)
      (setq rlt nil)))
    rlt))

(setq url-privacy-level 'high)
;; (setq url-privacy-level 'none)
;; (setq url-privacy-level 'none)
;; (setq url-privacy-level 'high)
;; (setq url-privacy-level 'paranoid)

;; func to check file metadata
(defun +file-metadata ()
  (interactive)
  (let* ((fname (buffer-file-name))
         (data (file-attributes fname))
         (access (current-time-string (nth 4 data)))
         (mod (current-time-string (nth 5 data)))
         (change (current-time-string (nth 6 data)))
         (size (nth 7 data))
         (mode (nth 8 data)))
    (message
     "%s:
  Accessed: %s
  Modified: %s
  Changed: %s
  Size: %s bytes
  Mode: %s"
     fname access mod change size mode)))

(use-package proced-narrow
  :straight t
  :after proced
  :commands (proced-narrow)
  :init
  (evilified-state-evilify proced-mode proced-mode-map
    "/" #'proced-narrow))

(with-eval-after-load 'proced
  (evilified-state-evilify proced-mode proced-mode-map
    "K" #'proced-send-signal
    "R" #'proced-renice
    "r" #'revert-buffer
    "/" #'proced-narrow))

(use-package terminal-here
  :straight t
  :commands (terminal-here terminal-here-launch terminal-here-project-launch)
  :config
  (defun terminal-here-default-terminal-command (_dir)
    "Pick a good default command to use for DIR."
    (cond
     ((eq system-type 'darwin)
      (list
       "/Applications/Alacritty.app/Contents/MacOS/alacritty"
       ;; "-e"
       ;; "setup-emacs-alacritty"
       "-t"
       "emacs-temp-alacritty"
       "--working-directory"
       (expand-file-name _dir)))

     ;; From http://stackoverflow.com/a/13509208/874671
     ((memq system-type '(windows-nt ms-dos cygwin))
      (list "cmd.exe" "/C" "start" "cmd.exe"))

     ;; Probably X11!
     (t '("x-terminal-emulator")))))

;; echo window shadow <cgwindowid> 0 | nc 127.0.0.1 5050

;; https://emacs.stackexchange.com/questions/47341/fine-grained-undo/47349#47349
(when (timerp undo-auto-current-boundary-timer)
  (cancel-timer undo-auto-current-boundary-timer))

(fset 'undo-auto--undoable-change
      (lambda ()
        (add-to-list 'undo-auto--undoably-changed-buffers (current-buffer))))

(fset 'undo-auto-amalgamate 'ignore)

(use-package beacon
  :straight t
  :defer t
  :disabled
  :diminish beacon-mode
  :init
  (setq beacon-blink-when-focused t)
  (setq beacon-blink-when-buffer-changes t)
  (beacon-mode 1)
  (push 'comint-mode beacon-dont-blink-major-modes))

(use-package auto-yasnippet
  :straight t
  :defer t
  :init
  (global-set-key (kbd "M-h") #'aya-create)
  (global-set-key (kbd "M-l") #'aya-expand))

(use-package so-long
  :straight (:repo "https://git.savannah.nongnu.org/git/so-long.git")
  :init
  (so-long-enable)
  (add-hook 'find-file-hook
            (defun my-find-file-care-about-long-lines ()
              (save-excursion
                (goto-char (point-min))
                (when (and (not (eq major-mode 'image-mode))
                           (search-forward-regexp ".\\{2000\\}" 50000 t)
                           (y-or-n-p
                            "Very long lines detected - enable so-long-mode? "))
                  (so-long-mode))))))

(use-package pcre2el
  :straight t
  :defer t
  :bind
  ("C-c / p /" . rxt-explain-pcre)
  ("C-c / /" . rxt-explain))

(use-package undo-propose
  :disabled
  :straight (:host github :repo "jackkamm/undo-propose-el")
  :commands (undo-propose))


(use-package ix
  :straight t
  :commands (ix))

(use-package webpaste
  :straight t
  :defer t
  :bind (("C-c C-p C-b" . webpaste-paste-buffer)
         ("C-c C-p C-r" . webpaste-paste-region))
  :config
  (progn
    (setq webpaste-provider-priority '("ptpb.pw" "dpaste.de" "ix.io"))))

;; use f5 as f1
(define-key key-translation-map [f5] [f1])

(use-package edit-indirect
  :straight t
  :commands (edit-indirect-region))

;; (yq/get-modules "scale-to-fit.el")
;; (spacemacs/set-leader-keys "tT" #'scale-to-fit-toggle)

(defun +major-mode-map ()
  (interactive)
  (symbol-value (read (concat (symbol-name major-mode) "-map"))))

(use-package define-word
  :straight t
  :commands (define-word))

(use-feature flyspell
  :defer t
  :hook (;;(prog-mode . flyspell-prog-mode)
         ((git-commit-mode org-mode text-mode markdown-mode) . flyspell-mode)))

(use-package auto-dictionary
  :straight t
  :defer t
  :hook (flyspell-mode . auto-dictionary-mode))

(use-package gcmh
  :straight t
  :diminish gcmh-mode
  :init
  (gcmh-mode 1))

(use-package whitespace-cleanup-mode
  :straight t
  :diminish whitespace-cleanup-mode
  :init
  (global-whitespace-cleanup-mode)
  (add-hook 'markdown-mode-hook (lambda () (whitespace-cleanup-mode -1))))

(use-package mic-paren
  :straight t
  :custom
  (paren-sexp-mode 'match)
  :init
  (setq +mic-paren-modes '(clojure-mode emacs-lisp-mode))
  (add-hook
   'buffer-list-update-hook
   (lambda ()
     (if (memq major-mode +mic-paren-modes)
         (paren-activate)
       (paren-deactivate)))))

(use-package fence-edit
  :straight (:host github :repo "aaronbieber/fence-edit.el")
  :commands (fence-edit-code-at-point fence-edit-dwim)
  :bind
  (:map yq-s-map ("z" . fence-edit-dwim))
  :config
  (add-to-list 'fence-edit-lang-modes '("css" . css-mode))
  (evil-define-key 'normal fence-edit-mode-map "sh" #'fence-edit-save)
  (setq
   styled-component-start
   (rx-to-string '(: (1+ (and (+ word) (0+ "\.") (0+ "(" (+ alpha) ")"))) "`" eol)))
  (setq styled-component-end (rx-to-string '(: "`;" eol)))

  (add-to-list 'fence-edit-blocks `(,styled-component-start ,styled-component-end css))
  (setq fence-edit-default-mode 'fundamental-mode))

;; Keychain stuff. Note to self: if you keep having to enter your
;; keychain password on OS X, make sure that you have the following in .ssh/config:
;; Host *
;;    UseKeychain yes
(use-package keychain-environment
  :straight t
  :hook (after-init . keychain-refresh-environment))

;; try undo-propose
(use-package undo-propose
  :straight t
  :disabled
  :init
  (define-key evil-normal-state-map "U" #'undo-propose)
  (evil-define-key 'normal undo-propose-mode-map "u" 'undo))

(use-feature xref
  :defer t
  :config
  (evil-define-key 'normal xref--xref-buffer-mode-map (kbd "RET") #'xref-quit-and-goto-xref)
  (define-key xref--xref-buffer-mode-map (kbd "RET") #'xref-quit-and-goto-xref)
  (define-key xref--button-map (kbd "RET") #'xref-quit-and-goto-xref)
  (evil-define-key 'normal xref--xref-buffer-mode-map (kbd "TAB") #'xref-goto-xref)
  (evil-define-key 'normal xref--xref-buffer-mode-map "l" #'xref-quit-and-goto-xref)
  (evil-define-key 'normal xref--xref-buffer-mode-map (kbd "q") #'quit-window))

(use-package powerthesaurus
  :straight t
  :commands
  (powerthesaurus-lookup-word
   powerthesaurus-lookup-word-at-point
   powerthesaurus-lookup-word-dwim))

(use-package discover-my-major
  :straight t
  :commands (discover-my-major))

(use-package cycle-quotes
  :straight t
  :commands (cycle-quotes)
  :bind ((:map yq-s-map) ("Q" . cycle-quotes)))

(use-package link-hint
  :straight t
  :commands (link-hint-open-link)
  :init
  (define-key evil-normal-state-map "go" #'link-hint-open-link)
  (evil-define-key 'normal helpful-mode-map "o" #'link-hint-open-link))

(use-package sudo-edit
  :straight t
  :commands (sudo-edit))

;; http://mbork.pl/2019-11-11_Diffing_buffer_fragments
(defun diff-last-two-kills ()
  "Write the last two kills to temporary files and diff them."
  (interactive)
  (let ((old "/tmp/old-kill") (new "/tmp/new-kill"))
    (with-temp-file new
      (insert (current-kill 0 t)))
    (with-temp-file old
      (insert (current-kill 1 t)))
    (diff old new "-u" t)))

(use-feature googlese-search
  :load-path "modules"
  :init
  (spacemacs/set-leader-keys "so" #'googlese-search))

(use-feature pullover
  :load-path "~/Dropbox/sync/pullover"
  :defer t
  :custom (pullover-major-mode #'gfm-mode)
  :config
  (define-key pullover-mode-map (kbd "C-c <C-return>") #'pullover-start-or-finish)
  (define-key pullover-mode-map (kbd "C-c C-k") #'pullover-cancel))

(defun +color-buffer (proc &rest args)
  (interactive)
  (with-current-buffer (process-buffer proc)
    (read-only-mode -1)
    (ansi-color-apply-on-region (point-min) (point-max))
    (read-only-mode 1)))

(use-package ctrlf
  :straight (:host github :repo "raxod502/ctrlf")
  :disabled)

(use-package side-notes
  :straight (:host github :repo "rnkn/side-notes")
  :bind ("s-i" . side-notes-toggle-notes)
  :custom (side-notes-file "notes.side.org"))

(defun xah-copy-file-path (&optional @dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

If in dired, copy the file/dir cursor is on, or marked files.

If a buffer is not file and not dired, copy value of `default-directory' (which is usually the “current” dir when that buffer was created)

URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2017-09-01"
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if @dir-path-only-p
         (progn
           (message "Directory path copied: 「%s」" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: 「%s」" $fpath)
         $fpath )))))
(spacemacs/set-leader-keys "fyp" 'xah-copy-file-path)

(defun +indent-between-pair (&rest _ignored)
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))
