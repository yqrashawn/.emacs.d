(defun yq/swiper-region-or-symbol ()
  "Run `swiper' with the selected region or the symbol
around point as the initial input."
  (interactive)
  (let ((input (if (region-active-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))
                 (thing-at-point 'symbol t))))
    (swiper input)))

(defun yq/swiper-all-region-or-symbol ()
  "Run `swiper-all' with the selected region or the symbol
around point as the initial input."
  (interactive)
  (ivy-read "Swiper: " (swiper--multi-candidates
                        (cl-remove-if-not
                         #'buffer-file-name
                         (buffer-list)))
            :initial-input (if (region-active-p)
                               (buffer-substring-no-properties
                                (region-beginning) (region-end))
                             (thing-at-point 'symbol t))
            :action 'swiper-multi-action-2
            :unwind #'swiper--cleanup
            :caller 'swiper-multi))

(use-package swiper
  :straight t
  :config
  (global-set-key (kbd "C-SPC") 'counsel-grep-or-swiper)
  (global-set-key (kbd "^@") 'counsel-grep-or-swiper)
  (global-set-key (kbd "C-S-SPC") 'yq/swiper-region-or-symbol))

(use-package counsel
  :straight t
  :diminish counsel-mode
  :config
  (counsel-mode 1)
  (setq counsel-find-file-occur-cmd "ls | grep -i -E '%s' | gxargs -d '\n' ls")
  (define-key ivy-minibuffer-map (kbd "C-n") 'ivy-next-history-element)
  (define-key ivy-minibuffer-map (kbd "C-p") 'ivy-previous-history-element)
  (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (spacemacs/set-leader-keys "<SPC>" 'counsel-M-x)
  (spacemacs/set-leader-keys "f" nil)
  (spacemacs/set-leader-keys "fe" nil)
  (spacemacs/set-leader-keys "fed" 'yq/edit-dotfile)
  (spacemacs/set-leader-keys "ff" 'counsel-find-file)
  (spacemacs/set-leader-keys "fF" 'find-file-other-window)
  (spacemacs/set-leader-keys "h" nil)
  (spacemacs/set-leader-keys "hd" nil)
  (spacemacs/set-leader-keys "hdf" 'counsel-describe-function)
  (spacemacs/set-leader-keys "hdv" 'counsel-describe-variable)
  (spacemacs/set-leader-keys "hdk" 'describe-key)
  (spacemacs/set-leader-keys "hdh" 'counsel-describe-symbol-history)
  (spacemacs/set-leader-keys "fJ" 'spacemacs/open-junk-file)
  (define-key evil-normal-state-map "sf" 'counsel-rg)
  (define-key evil-normal-state-map "sl" 'counsel-imenu)
  (define-key evil-normal-state-map "sL" 'spacemacs/swiper-all-region-or-symbol)
  (define-key evil-normal-state-map "sj" 'counsel-recentf))

(straight-use-package 'counsel-tramp)

(use-package ivy
  :straight t
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-height 16)
  (defun yq/ivy-evil-registers ()
    "Show evil registers"
    (interactive)
    (let ((ivy-height 24))
      (ivy-read "Evil Registers:"
                (cl-loop for (key . val) in (evil-register-list)
                         collect (eval `(format "%s : %s" (propertize ,(char-to-string key) 'face 'font-lock-builtin-face)
                                                ,(or (and val
                                                          (stringp val)
                                                          (replace-regexp-in-string "\n" "^J" val))
                                                     ""))))
                :action #'yq/ivy-insert-evil-register)))

  (defun yq/ivy-insert-evil-register (candidate)
    (insert (replace-regexp-in-string "\\^J" "\n"
                                      (substring-no-properties candidate 4))))

  (setq ivy-use-selectable-prompt t)
  (ivy-set-occur 'spacemacs/counsel-search
                 'spacemacs//counsel-occur)
  (evil-make-overriding-map ivy-occur-mode-map 'normal)
  (define-key evil-normal-state-map "sb" 'ivy-switch-buffer))

(use-package smex
  :straight t
  :init
  (setq-default smex-history-length 32)
  (setq-default smex-save-file (concat yq-emacs-cache-dir ".smex-items")))

(use-package projectile
  :straight t
  :diminish projectile-mode)

;; TODO: config action
(use-package counsel-projectile
  :straight t
  :config
  (counsel-projectile-mode)
  (spacemacs/set-leader-keys "p" nil)
  (spacemacs/set-leader-keys "pb" 'counsel-projectile)
  (spacemacs/set-leader-keys "pf" 'counsel-projectile-find-file)
  (spacemacs/set-leader-keys "pd" 'counsel-projectile-find-dir)
  (spacemacs/set-leader-keys "pl" 'counsel-projectile-switch-project)
  (spacemacs/set-leader-keys "ps" 'counsel-projectile-rg))

(yq/get-modules "counsel-funcs.el")
(spacemacs/set-leader-keys "s" nil)
(spacemacs/set-leader-keys "sf" 'spacemacs/search-auto)
(spacemacs/set-leader-keys "sF" 'spacemacs/search-auto-region-or-symbol)
;; (define-key evil-normal-state-map "sf" 'spacemacs/search-rg-direct)
(define-key evil-normal-state-map "sF" 'spacemacs/search-auto-region-or-symbol-direct)

(use-package dired-narrow
  :straight t
  :after dired
  :commands (dired-narrow-fuzzy))

(use-package dired
  :config
  (evil-define-key 'normal dired-mode-map
    "s" 'nil
    "sk" 'yq/kill-this-buffer
    "sf" 'counsel-rg
    "ss" 'dired-sort-toggle-or-edit
    "sc" 'yq/delete-window
    "f" 'dired-narrow-fuzzy))

(use-package dired+
  :straight t
  :init
  (setq diredp-hide-details-initially-flag nil)
  (evil-leader/set-key "fj" 'diredp-dired-recent-dirs)
  (evil-leader/set-key "fJ" 'diredp-dired-recent-dirs-other-window)
  (evil-define-key 'normal dired-mode-map "h" 'diredp-up-directory-reuse-dir-buffer)
  (evil-define-key 'normal dired-mode-map "j" 'diredp-next-line)
  (evil-define-key 'normal dired-mode-map "k" 'diredp-previous-line)
  (evil-define-key 'normal dired-mode-map "l" 'diredp-find-file-reuse-dir-buffer))

(use-package dired-x
  :hook (dired-mode . dired-omit-mode)
  :commands (dired-jump
             dired-jump-other-window
             dired-omit-mode)
  :config
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\.DS_Store$\\|^__MACOSX$\\|^\\.localized$")))

(defcustom counsel-fd-base-command "fd -L -I --hidden -a --color never "
  "Alternative to `counsel-fd-base-command' using ripgrep."
  :type 'string
  :group 'ivy)
;; (setq counsel-fd-base-command "fd -L -I --hidden -p --color never ")

(defun counsel-fd-function (string base-cmd)
  "Grep in the current directory for STRING using BASE-CMD.
If non-nil, append EXTRA-fd-ARGS to BASE-CMD."

  (if (< (length string) 3)
      (counsel-more-chars 3)
    (let ((default-directory counsel-fd-current-dir)
          (regex (counsel-unquote-regex-parens
                  (setq ivy--old-re
                        (ivy--regex-plus string)))))
      (let* ((fd-cmd (concat (format base-cmd) (concat " " (s-wrap regex "'")))))
        (counsel--async-command fd-cmd)
        nil))))

(defun my-insert-fd-full-path (path)
  (insert (concat counsel-fd-current-dir path)))

(defun my-insert-tsfile-path (path)
  (insert (concat (concat "[[tsfile:" (f-filename path)) "][]]")))

(defun counsel-fd (&optional initial-input initial-directory fd-prompt)
  "Grep for a string in the current directory using fd.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-FD-ARGS string, if non-nil, is appended to `counsel-fd-base-command'.
FD-PROMPT, if non-nil, is passed as `ivy-read' prompt argument."
  (interactive
   (list nil
         (when current-prefix-arg
           (read-directory-name (concat
                                 (car (split-string counsel-fd-base-command))
                                 " in directory: ")))))
  (counsel-require-program (car (split-string counsel-fd-base-command)))
  (ivy-set-prompt 'counsel-fd counsel-prompt-function)
  (setq counsel-fd-current-dir (or initial-directory default-directory))
  (ivy-read (or fd-prompt (car (split-string counsel-fd-base-command)))
            (lambda (string)
              (counsel-fd-function string counsel-fd-base-command))
            :initial-input initial-input
            :dynamic-collection t
            ;; :keymap counsel-ag-map
            :history 'counsel-git-grep-history
            :action '(1 ("z" (lambda (file)
                               (with-ivy-window
                                 (when file
                                   (find-file  file)))))
                        ("o" (lambda (path)
                               (my-insert-tsfile-path path)
                               (backward-char)
                               (backward-char)) "insert tsfile path"))
            :unwind (lambda ()
                      (counsel-delete-process)
                      (swiper--cleanup))
            :caller 'counsel-fd))

(spacemacs/set-leader-keys "sm" 'counsel-fd)

(use-package open-junk-file
  :straight t
  :defer t
  :commands (open-junk-file)
  :init
  (setq open-junk-file-format (concat spacemacs-cache-directory "junk/%Y/%m/%d-%H%M%S."))
  (defun spacemacs/open-junk-file (&optional arg)
    "Open junk file using helm or ivy.

Interface choice depends on whether the `ivy' layer is used or
not.

When ARG is non-nil search in junk files."
    (interactive "P")
    (let* ((fname (format-time-string open-junk-file-format (current-time)))
           (rel-fname (file-name-nondirectory fname))
           (junk-dir (file-name-directory fname))
           (default-directory junk-dir))
      (cond ((and arg (configuration-layer/layer-used-p 'ivy))
             (spacemacs/counsel-search dotspacemacs-search-tools nil junk-dir))
            (t
             (counsel-find-file rel-fname)))))
  (spacemacs/set-leader-keys "fJ" 'spacemacs/open-junk-file))