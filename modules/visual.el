(yq/get-modules "visual-funcs.el")
(spacemacs/set-leader-keys "tf" 'yq/toggle-default-font)

(use-package ivy-rich
  :straight t
  :after counsel
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :init
  (ivy-rich-mode))

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
  :disabled
  :defer t)

(use-package zenburn-theme
  :straight t
  :init
  (setq yq/dark-theme 'zenburn))

(use-package doom-themes
  :straight t
  :disabled
  :defer t
  :init (setq yq/light-theme 'doom-nord-light))

(use-package spacemacs-theme
  :straight t
  :defer t
  :init
  (setq yq/light-theme 'spacemacs-light))

(defvar spacemacs-evil-cursors '(("normal" "DarkGoldenrod2" box)
                                 ("insert" "chartreuse3" (bar . 4))
                                 ("emacs" "SkyBlue2" box)
                                 ("hybrid" "SkyBlue2" (bar . 4))
                                 ("replace" "chocolate" (hbar . 4))
                                 ("evilified" "LightGoldenrod3" box)
                                 ("visual" "gray" (hbar . 4))
                                 ("motion" "plum3" box)
                                 ("lisp" "HotPink1" box)
                                 ("iedit" "firebrick1" box)
                                 ("iedit-insert" "firebrick1" (bar . 4)))
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
       (list color shape)))

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

(defun +set-mic-paren-face ()
  ;; mic-paren
  (set-face-foreground 'paren-face-match nil)
  (set-face-background 'paren-face-match "#506575")
  (set-face-background 'paren-face-mismatch "#DC8CC3")
  (set-face-background 'paren-face-no-match "#CC9393"))
(add-hook 'spacemacs-post-theme-change-hook
          (lambda ()
            (cond
             ((and (memq yq/current-theme '(zenburn modus-vivendi)) (facep 'paren-face-match))
              (+set-mic-paren-face))
             ((memq yq/current-theme '(modus-operandi spacemacs-light))
              (set-face-background 'paren-face-match "#dbd9d1")))))

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
  (setq yq/current-theme theme)
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

(use-package all-the-icons
  :straight t
  :defer t)
;; (all-the-icons-install-fonts)

(use-package minions
  :straight t
  :disabled
  :init (minions-mode))

(use-package doom-modeline
  :straight t
  :custom
  (doom-modeline-height 15)
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-icon (display-graphic-p))
  (doom-modeline-mu4e t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-indent-info t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-env-version t)
  (doom-modeline-indent-info t)
  (doom-modeline-irc nil)
  (doom-modeline-word-count t)
  (doom-modeline-percent-position nil)
  :config
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches buffer-info remote-host selection-info)
    '(misc-info persp-name battery grip irc mu4e gnus github debug lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker))
  (doom-modeline-init))

(use-package vim-empty-lines-mode
  :straight (:host github :repo "jmickelin/vim-empty-lines-mode")
  :disabled
  :init
  (global-vim-empty-lines-mode))

(use-package highlight-blocks
  :straight t
  :disabled t
  :hook (prog-mode . highlight-blocks-mode))

(use-package symbol-overlay
  :straight t
  :diminish symbol-overlay-mode
  :commands (symbol-overlay-put)
  :hook (prog-mode . symbol-overlay-mode)
  :init
  (define-key evil-normal-state-map "gu" 'symbol-overlay-put)
  (yq/add-toggle symbol-overlay :mode symbol-overlay-mode)
  (spacemacs/set-leader-keys "tH" 'yq/toggle-symbol-overlay)
  :config
  (setq symbol-overlay-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "i") 'symbol-overlay-put)
          (define-key map (kbd "p") 'symbol-overlay-jump-prev)
          (define-key map (kbd "n") 'symbol-overlay-jump-next)
          (define-key map (kbd "<") 'symbol-overlay-jump-first)
          (define-key map (kbd ">") 'symbol-overlay-jump-last)
          map)))

(use-package pretty-magit
  :load-path "~/.emacs.d/modules"
  :after magit
  :config
  (pretty-magit-setup)
  (pretty-magit-add-leaders
   '(("Add"     ? (:foreground "#375E97" :height 1.2))
     ("Feature" ? (:foreground "slate gray" :height 1.2))
     ("Fix"     ? (:foreground "#FB6542" :height 1.2))
     ("Clean"   ? (:foreground "#FFBB00" :height 1.2))
     ("Docs"    ? (:foreground "#3F681C" :height 1.2))
     ("Test"    ?T (:foreground "#3F681C" :height 1.2)))))

(use-package pretty-fonts
  :disabled
  :load-path "~/.emacs.d/modules"
  :config
  ;; !! This is required to avoid segfault when using emacs as daemon !!
  (spacemacs|do-after-display-system-init
   (pretty-fonts-add-hook 'prog-mode-hook pretty-fonts-fira-code-alist)
   (pretty-fonts-add-hook 'org-mode-hook  pretty-fonts-fira-code-alist)

   (pretty-fonts-set-fontsets-for-fira-code)
   (pretty-fonts-set-fontsets
    '(;; All-the-icons fontsets
      ("fontawesome"
       ;;                         
       #xf07c #xf0c9 #xf0c4 #xf0cb #xf017 #xf101)

      ("all-the-icons"
       ;;    
       #xe907 #xe928)

      ("github-octicons"
       ;;                               
       #xf091 #xf059 #xf076 #xf075 #xe192  #xf016 #xf071)

      ("material icons"
       ;;              
       #xe871 #xe918 #xe3e7  #xe5da
       ;;              
       #xe3d0 #xe3d1 #xe3d2 #xe3d4)))))

(use-package pretty-mode
  :straight t
  :disabled
  :init
  (global-pretty-mode t)
  :config
  (pretty-deactivate-patterns :Rightarrow)
  (pretty-deactivate-patterns :twoheadrightarrow)
  (pretty-deactivate-groups
   '(:equality :ordering :ordering-double :ordering-triple
               :arrows :arrows-twoheaded :punctuation
               :logic :sets))

  (pretty-activate-groups
   '(:sub-and-superscripts :greek :arithmetic-nary)))

(use-package highlight-indent-guides
  :disabled
  :straight t
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-character ?\|)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-delay 1))

(use-package fill-column-indicator
  :straight t
  :hook ((text-mode markdown-mode org-mode ) . fci-mode))

;; visual feedback for evil-ex command
(use-package evil-traces
  :straight t
  :disabled
  :after evil
  :config
  (evil-traces-use-diff-faces) ; if you want to use diff's faces
  (evil-traces-mode))

(use-package foldit
  :load-path "./foldit.el"
  :disabled
  :after (hideshow)
  :config
  (foldit-global-mode))

(use-package solaire-mode
  :straight t
  :disabled
  :init (solaire-global-mode))

(use-package highlight-escape-sequences
  :straight t
  :disabled
  :init (hes-mode))

;; https://www.manueluberti.eu//emacs/2020/03/16/modus-themes/
(use-package modus-operandi-theme
  :straight t
  :defer t)
(use-package modus-vivendi-theme
  :straight t
  :defer t)
;; (load-theme 'modus-vivendi t)
;; (load-theme 'modus-operandi t)


(use-package fira-code-mode
  :straight t
  :disabled t
  :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x")) ;; List of ligatures to turn off
  :hook prog-mode)

(use-package backline
  :straight t
  :after outline
  :config (advice-add 'outline-flag-region :after 'backline-update))

(use-package outline-minor-faces
  :straight t
  :disabled t
  :after outline
  :config (add-hook 'outline-minor-mode-hook
                    'outline-minor-faces-add-font-lock-keywords))