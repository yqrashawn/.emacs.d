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
  (spacemacs/set-leader-keys "h" nil)
  (spacemacs/set-leader-keys "hd" nil)
  (spacemacs/set-leader-keys "hdf" 'counsel-describe-function)
  (spacemacs/set-leader-keys "hdv" 'counsel-describe-variable)
  (spacemacs/set-leader-keys "hdk" 'describe-key)
  (spacemacs/set-leader-keys "hdh" 'counsel-describe-symbol-history)
  (define-key evil-normal-state-map "sl" 'counsel-imenu)
  (define-key evil-normal-state-map "sj" 'counsel-recentf))

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
(define-key evil-normal-state-map "sf" 'spacemacs/search-auto)
(define-key evil-normal-state-map "sF" 'spacemacs/search-auto-region-or-symbol)

(use-package dired-narrow
  :straight t
  :commands (dired-narrow-fuzzy))

(use-package dired
  :config
  (evil-define-key 'normal dired-mode-map
    "l" 'dired-find-file
    "f" 'dired-narrow-fuzzy
    "h" 'dired-up-directory))

(use-package dired-x
  :commands (dired-jump
             dired-jump-other-window
             dired-omit-mode))
