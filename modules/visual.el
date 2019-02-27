(yq/get-modules "visual-funcs.el")
(spacemacs/set-leader-keys "tf" 'yq/toggle-default-font)

(use-package ivy-rich
  :straight t
  :after ivy
  :init (ivy-rich-mode 1))

(use-package golden-ratio-scroll-screen
  :straight t
  :after evil
  :config
  (evil-set-command-property 'golden-ratio-scroll-screen-up :jump t)
  (evil-set-command-property 'golden-ratio-scroll-screen-down :jump t)
  (setq golden-ratio-scroll-highlight-delay (quote (0.07 . 0.03)))
  (setq golden-ratio-scroll-highlight-flag (quote (quote nil)))
  (define-key evil-normal-state-map (kbd "C-d") 'golden-ratio-scroll-screen-up)
  (define-key evil-normal-state-map (kbd "C-u") 'golden-ratio-scroll-screen-down))

(use-package mode-line-bell
  :straight t
  :config (mode-line-bell-mode))

(use-package highlight-parentheses
  :straight t
  :diminish highlight-parentheses-mode
  :defer t
  :hook (prog-mode . highlight-parentheses-mode))

(use-package leuven-theme
  :straight t
  :disabled
  :defer t)

(use-package apropospriate-theme
  :straight (:host github :repo "waymondo/apropospriate-theme")
  :defer t)

(use-package zenburn-theme
  :straight t
  :defer t
  :init (setq yq/dark-theme 'zenburn))

(use-package doom-themes
  :straight t
  :defer t
  :init (setq yq/light-theme 'doom-nord-light))

;; (load-theme 'yq-default-emacs-theme)
;; (load-theme 'default-white)
;; (load-theme 'zenburn)

(defvar dotspacemacs-colorize-cursor-according-to-state t
  "If non nil the cursor color matches the state color in GUI Emacs.")

(defvar spacemacs-evil-cursors '(("normal" "DarkGoldenrod2" box)
                                 ("insert" "chartreuse3" (bar . 2))
                                 ("emacs" "SkyBlue2" box)
                                 ("hybrid" "SkyBlue2" (bar . 2))
                                 ("replace" "chocolate" (hbar . 2))
                                 ("evilified" "LightGoldenrod3" box)
                                 ("visual" "gray" (hbar . 2))
                                 ("motion" "plum3" box)
                                 ("lisp" "HotPink1" box)
                                 ("iedit" "firebrick1" box)
                                 ("iedit-insert" "firebrick1" (bar . 2)))
  "Colors assigned to evil states with cursor definitions.
To add your own, use `spacemacs/add-evil-curosr'.")

(defun spacemacs/add-evil-cursor (state color shape)
  "Define a cursor and face for a new evil state.
An appropriate entry is added to `spacemacs-evil-cursors', as well.

For evil states that do not need an evil cursor use
`spacemacs/define-evil-state-face' instead."
  (add-to-list 'spacemacs-evil-cursors (list state color shape))
  (spacemacs/define-evil-state-face state color)
  (set (intern (format "evil-%s-state-cursor" state))
       (list (when dotspacemacs-colorize-cursor-according-to-state color)
             shape)))

(defun spacemacs/define-evil-state-face (state color)
  "Define a face for an evil state.
For evil states that also need an entry to `spacemacs-evil-cursors' use
`spacemacs/add-evil-cursor' instead."
  ;; this function and `spacemacs/add-evil-cursor' need to be separate because
  ;; some states must explicitly *not* have their own evil spacemacs cursor
  ;; for example treemacs: it needs no cursor since it solely uses hl-line-mode
  ;; and having an evil cursor defined anyway leads to the cursor sometimes
  ;; visibly flashing in treemacs buffers
  (eval `(defface ,(intern (format "spacemacs-%s-face" state))
           `((t (:background ,color
                             :foreground ,(face-background 'mode-line)
                             :inherit 'mode-line)))
           (format "%s state face." state)
           :group 'spacemacs)))

(defvar spacemacs--cur-theme nil
  "Internal variable storing currently loaded theme.")

(defvar spacemacs-post-theme-change-hook nil
  "Hook run after theme has changed.")

(defadvice load-theme (after spacemacs/load-theme-adv activate)
  "Perform post load processing."
  (let ((theme (ad-get-arg 0)))
    ;; Without this a popup is raised every time emacs25 starts up for
    ;; assignment to a free variable
    (with-no-warnings
      (setq spacemacs--cur-theme theme))
    (spacemacs/post-theme-init theme)))

(defun spacemacs/post-theme-init (theme)
  "Some processing that needs to be done when the current theme
has been changed to THEME."
  (interactive)
  (run-hooks 'spacemacs-post-theme-change-hook))

(cl-loop for (state color shape) in spacemacs-evil-cursors
         do (spacemacs/add-evil-cursor state color shape))

(defun spacemacs/set-state-faces ()
  (cl-loop for (state color cursor) in spacemacs-evil-cursors
           do
           (set-face-attribute (intern (format "spacemacs-%s-face" state))
                               nil
                               :foreground (face-background 'mode-line))))

(add-hook 'spacemacs-post-theme-change-hook 'spacemacs/set-state-faces)

(defun spacemacs//adaptive-evil-highlight-persist-face ()
  (set-face-attribute 'evil-search-highlight-persist-highlight-face nil
                      :inherit 'lazy-highlight
                      :background nil
                      :foreground nil))
(spacemacs//adaptive-evil-highlight-persist-face)
(add-hook 'spacemacs-post-theme-change-hook 'spacemacs//adaptive-evil-highlight-persist-face)

;; (use-package all-the-icons
;;   :straight t)
;; (straight-use-package 'all-the-icons)

(use-package minions
  :straight t
  :disabled
  :init (minions-mode))

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-init)
  :init (setq doom-modeline-height 15
              doom-modeline-icon nil
              doom-modeline-buffer-file-name-style 'file-name
              doom-modeline-minor-modes t
              doom-modeline-env-version t
              doom-modeline-lsp nil)
  :config
  (doom-modeline-def-segment tabbar-group
    (when (and tabbar-mode (doom-modeline--active))
      (let ((tb-groups (+tabbar-get-groups))
            (tb-cur-group (first (+tabbar-buffer-groups))))
        (mapcar
         (lambda (group)
           (+propertize-tabbar-group-for-modeline group (eq group tb-cur-group))) tb-groups))))
  (defun +propertize-tabbar-group-for-modeline (group &optional cur-group-p)
    (let ((face (if cur-group-p 'doom-modeline-evil-emacs-state 'mode-line-emphasis))
          (active (doom-modeline--active)))
      (concat
       " "
       (propertize group
                   'face face
                   'help-echo "Tabbar Group")
       " ")))
  (doom-modeline-def-modeline 'main
    '(bar workspace-number window-number evil-state matches tabbar-group buffer-info  remote-host buffer-position parrot selection-info)
    '(misc-info persp-name lsp irc mu4e github debug buffer-encoding major-mode process vcs checker)))