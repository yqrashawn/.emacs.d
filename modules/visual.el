;;; visual
(yq/get-modules "visual-funcs.el")
;; (spacemacs/set-leader-keys "tf" 'yq/toggle-default-font)

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
  (setq yq/dark-theme 'modus-vivendi))

(use-package doom-themes
  :straight t
  :disabled
  :defer t)

(use-package spacemacs-theme
  :straight t
  :defer t
  :disabled t
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

(use-package mic-paren
  :straight t
  :custom
  (paren-display-message 'only)
  (paren-sexp-mode 'match)
  :init
  (setq +mic-paren-modes '(clojure-mode clojurescript-mode emacs-lisp-mode))
  (add-hook 'buffer-list-update-hook
            (lambda ()
              (if (memq major-mode +mic-paren-modes)
                  (paren-activate)
                (paren-deactivate))))

  (defun +set-mic-paren-face-for-dark-bg ()
    ;; mic-paren
    (set-face-bold 'paren-face-match nil)
    (set-face-background 'paren-face-match "#2E4353")
    (set-face-background 'paren-face-mismatch "#DC8CC3")
    (set-face-background 'paren-face-no-match "#CC9393"))
  (defun +set-mic-paren-face-for-light-bg ()
    (set-face-background 'paren-face-match "#eceae2")))

(add-hook 'spacemacs-post-theme-change-hook
          (lambda ()
            (cond
             ((and (memq yq/current-theme '(zenburn modus-vivendi)) (facep 'paren-face-match))
              (+set-mic-paren-face-for-dark-bg))
             ((memq yq/current-theme '(modus-operandi))
              (+set-mic-paren-face-for-light-bg)))))

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
  (doom-modeline-buffer-file-name-style 'auto)
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
  (doom-modeline-percent-position t)
  :config
  ;; (doom-modeline-def-modeline 'main
  ;;   '(bar workspace-name window-number modals matches buffer-info remote-host selection-info)
  ;;   '(misc-info persp-name battery grip irc mu4e gnus github debug lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker))
  ;; https://github.com/seagle0128/doom-modeline#faq
  (setq inhibit-compacting-font-caches t)
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
  :disabled t
  :after magit
  :config
  (pretty-magit-setup)
  (pretty-magit-add-leaders
   '(("feat" ?✨ (:foreground "slate gray" :height 1.2))
     ("fix" ?🐛 (:foreground "#FB6542" :height 1.2))
     ("chore" ?📦 (:foreground "#3F681C" :height 1.2))
     ("test" ?🚨 (:foreground "#3F681C" :height 1.2))
     ("ci" ?🐳 (:foreground "#375E97" :height 1.2))
     ("docs" ?📓 (:foreground "#3F681C" :height 1.2))
     ("build" ?👷 (:foreground "#375E97" :height 1.2))
     ("pref" ?🐎 (:foreground "#FB6542" :height 1.2))
     ("refactor" ?🔨 (:foreground "#FFBB00" :height 1.2))
     ("revert" ?♻ (:foreground "#FFBB00" :height 1.2))
     ("style" ?💄 (:foreground "#FFBB00" :height 1.2))
     )))

(use-package pretty-fonts
  :disabled t
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
  :straight (:host github :repo "hlissner/highlight-escape-sequences")
  :hook (after-init . highlight-escape-sequences-mode))

;; https://www.manueluberti.eu//emacs/2020/03/16/modus-themes/
(use-package modus-operandi-theme
  :straight t
  :defer t
  :custom
  (modus-operandi-theme-slanted-constructs t)
  (modus-operandi-theme-bold-constructs t)
  (modus-operandi-theme-proportional-fonts t)
  (modus-operandi-theme-variable-pitch-headings t)
  (modus-operandi-theme-section-headings t)
  (modus-operandi-theme-scale-headings t)
  (modus-operandi-theme-visible-fringes t)
  (modus-operandi-theme-fringes 'subtle)
  (modus-operandi-theme-distinct-org-blocks t)
  (modus-operandi-theme-org-blocks 'grayscale)
  (modus-operandi-theme-mode-line '3d)
  (modus-operandi-theme-syntax 'alt-syntax)
  :init
  (setq yq/light-theme 'modus-operandi))
(use-package modus-vivendi-theme
  :straight t
  :defer t
  :custom
  (modus-vivendi-theme-slanted-constructs t)
  (modus-vivendi-theme-bold-constructs t)
  (modus-vivendi-theme-proportional-fonts t)
  (modus-vivendi-theme-variable-pitch-headings t)
  (modus-vivendi-theme-section-headings t)
  (modus-vivendi-theme-scale-headings t)
  (modus-vivendi-theme-visible-fringes t)
  (modus-vivendi-theme-fringes 'subtle)
  (modus-vivendi-theme-distinct-org-blocks t)
  (modus-vivendi-theme-org-blocks 'grayscale)
  (modus-vivendi-theme-mode-line '3d)
  (modus-vivendi-theme-syntax 'alt-syntax)
  :init
  (setq yq/dark-theme 'modus-vivendi))
(load-theme 'modus-vivendi t)
;; (load-theme 'modus-operandi t)


(use-package fira-code-mode
  :straight t
  :disabled t
  :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x")) ;; List of ligatures to turn off
  :hook prog-mode)

;; (use-package backline
;;   :straight t
;;   :after outshine
;;   :config (advice-add 'outshine-flag-subtree :after 'backline-update))

;; (use-package outline-minor-faces
;;   :straight t
;;   :after outshine
;;   :config (add-hook 'outshine-mode
;;                     'outline-minor-faces-add-font-lock-keywords))

(use-package emojify
  :straight t
  :commands (emojify-mode))

(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
  ;; :disabled t
  :hook (after-init . global-ligature-mode)
  :config
  (ligature-set-ligatures t '("[ERROR]" "[DEBUG]" "[INFO]" "[WARN]" "[WARNING]"
                              "[ERR]" "[FATAL]" "[TRACE]" "[FIXME]" "[TODO]"
                              "[BUG]" "[NOTE]" "[HACK]" "[MARK]"
                              "# ERROR" "# DEBUG" "# INFO" "# WARN" "# WARNING"
                              "# ERR" "# FATAL" "# TRACE" "# FIXME" "# TODO"
                              "# BUG" "# NOTE" "# HACK" "# MARK"
                              "// ERROR" "// DEBUG" "// INFO" "// WARN" "// WARNING"
                              "// ERR" "// FATAL" "// TRACE" "// FIXME" "// TODO"
                              "// BUG" "// NOTE" "// HACK" "// MARK"
                              "!!" "!=" "!==" "!!!" "!≡" "!≡≡" "!>" "!=<" "#("
                              "#_" "#{" "#?" "#>" "##" "#_(" "%=" "%>" "%>%" "%<%"
                              "&%" "&&" "&*" "&+" "&-" "&/" "&=" "&&&" "&>" "$>"
                              "***" "*=" "*/" "*>" "++" "+++" "+=" "+>" "++=" "--"
                              "-<" "-<<" "-=" "->" "->>" "---" "-->" "-+-" "-\\/"
                              "-|>" "-<|" ".." "..." "..<" ".>" ".~" ".=" "/*" "//"
                              "/>" "/=" "/==" "///" "/**" ":::" "::" ":=" ":≡" ":>"
                              ":=>" ":(" ":-(" ":)" ":-)" ":/" ":\\" ":3" ":D" ":P"
                              ":>:" ":<:" "<$>" "<*" "<*>" "<+>" "<-" "<<" "<<<" "<<="
                              "<=" "<=>" "<>" "<|>" "<<-" "<|" "<=<" "<~" "<~~" "<<~"
                              "<$" "<+" "<!>" "<@>" "<#>" "<%>" "<^>" "<&>" "<?>" "<.>"
                              "</>" "<\\>" "<\">" "<:>" "<~>" "<**>" "<<^" "<!" "<@"
                              "<#" "<%" "<^" "<&" "<?" "<." "</" "<\\" "<\"" "<:" "<->"
                              "<!--" "<--" "<~<" "<==>" "<|-" "<<|" "<-<" "<-->" "<<=="
                              "<==" "=<<" "==" "===" "==>" "=>" "=~" "=>>" "=/=" "=~="
                              "==>>" "≡≡" "≡≡≡" "≡:≡" ">-" ">=" ">>" ">>-" ">>=" ">>>"
                              ">=>" ">>^" ">>|" ">!=" ">->" "??" "?~" "?=" "?>" "???"
                              "?." "^=" "^." "^?" "^.." "^<<" "^>>" "^>" "\\\\" "\\>"
                              "\\/-" "@>" "|=" "||" "|>" "|||" "|+|" "|->" "|-->" "|=>"
                              "|==>" "|>-" "|<<" "||>" "|>>" "|-" "||-" "~=" "~>" "~~>"
                              "~>>" "[[" "]]" "\">" "_|_"))
  (global-ligature-mode t))

;; try prot-fonts https://protesilaos.com/dotemacs/#h:e03b6415-a18f-4058-b9b0-5721d38c6c50
(use-package prot-fonts
  :load-path "~/.emacs.d/modules"
  ;; :after (modus-vivendi-theme modus-operandi-theme)
  :init
  (setq prot-fonts-typeface-sets-alist
        '((laptop . (120 "PragmataPro Mono Liga" "DejaVu Sans"))
          (desktop . (135 "PragmataPro Mono Liga" "Inter"))
          (reader . (150 "PragmataPro Mono Liga" ;; "Iosevka"
                     "FiraGO"))
          (presentation . (190 "PragmataPro Mono Liga" ;; "Iosevka"
                           "FiraGO"))))
  (setq prot-fonts-monospaced-list
        '("PragmataPro Mono Liga" "DejaVu Sans Mono" "Iosevka" "Source Code Pro"
          "Ubuntu Mono" "Fantasque Sans Mono" "Fira Code" "Monoid"))
  (setq prot-fonts-heights-list
        '(100 105 110 120 130 140 150 160))
  (setq prot-fonts-line-spacing-alist
        '(("Source Code Pro" . 1)
          ("Ubuntu Mono" . 2)))
  (setq prot-fonts-laptop-desktop-keys-list '(laptop desktop))
  (setq prot-fonts-max-small-resolution-width 2048)
  (setq  prot-fonts-bold-weight-alist
         '(("Iosevka" . semibold)
           ("Source Code Pro" . semibold)))
  (spacemacs/set-leader-keys "tf" #'prot-fonts-set-fonts-dwim)
  :config
  ;; This is defined in Emacs' C code, though I feel this is a good
  ;; place to put it.
  (setq x-underline-at-descent-line t)
  ;; And this just sets the right font depending on whether my laptop is
  ;; connected to an external monitor or not.
  (prot-fonts-fonts-per-monitor)
  :hook ((prot-fonts-set-typeface-hook . prot-fonts-line-spacing)
         (prot-fonts-set-typeface-hook . prot-fonts-bold-face)
         (after-init . prot-fonts-fonts-per-monitor)
         ;; See theme section for this hook
         ;; (modus-themes-after-load-theme-hook . prot-fonts-bold-face)
         ))

(use-package prettify-utils
  :straight (:host github :repo "Ilazki/prettify-utils.el"))

(use-feature prog-mode
  :init
  (prettify-utils-add-hook org-mode
                           ("TODO:" "# TODO")
                           ("ERR:" "# ERR")
                           ("TRACE:" "# TRACE")
                           ("FATAL:" "# FATAL")
                           ("WARN:" "# WARN")
                           ("HACK:" "# HACK")
                           ("NOTE:" "# NOTE")
                           ("WARNING:" "# WARNING")
                           ("INFO:" "# INFO")
                           ("MARK:" "# MARK")
                           ("FIXME:" "# FIXME"))
  :hook (after-init . global-prettify-symbols-mode))