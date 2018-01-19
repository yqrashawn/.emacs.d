(defvar dotspacemacs-auto-save-file-location 'cache
  "Location where to auto-save files. Possible values are `original' to
auto-save the file in-place, `cache' to auto-save the file to another
file stored in the cache directory and `nil' to disable auto-saving.")
(defconst spacemacs-auto-save-directory
  (expand-file-name (concat yq-cache-directory "auto-save/"))
  "Spacemacs auto-save directory")
(defconst spacemacs-assets-directory
  (expand-file-name (concat yq-start-directory "assets/"))
  "Spacemacs assets directory.")
(defconst spacemacs-test-directory
  (expand-file-name (concat yq-start-directory "tests/"))
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
(blink-cursor-mode 0))
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
(tool-bar-mode -1)
(toggle-scroll-bar -1) 
(menu-bar-mode -1) 

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
