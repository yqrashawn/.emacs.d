(yq/get-modules "evil-core-funcs.el")
(yq/get-modules "core-jump.el")
;; (yq/get-modules "core-keybindings.el")
(global-set-key (kbd "C-g") 'keyboard-quit)
(global-set-key (kbd "C-x 1") 'spacemacs/toggle-maximize-buffer)

(use-package undo-tree
  :straight (:host github :repo "emacsorphanage/undo-tree")
  :diminish undo-tree-mode
  :defer t
  :custom
  (undo-tree-enable-undo-in-region nil)
  (undo-tree-history-directory-alist '(("." . "~/emacs.d/.cache/undo")))
  (undo-tree-auto-save-history t)
  :bind ("s-y" . undo-tree-redo)
  :config
  (setq undo-limit 78643200)
  (setq undo-outer-limit 104857600)
  (setq undo-strong-limit 157286400)
  (global-undo-tree-mode))

(use-package goto-chg
  :straight (:host github :repo "emacs-evil/goto-chg")
  :bind
  ("C-x C-8 [" . goto-last-change)
  ("C-x C-8 ]" . goto-last-change-reverse))

(use-package evil-leader
  :straight t
  :custom
  (evil-leader/in-all-states t)
  :init
  (global-evil-leader-mode)
  (defalias 'spacemacs/set-leader-keys 'evil-leader/set-key)
  (defalias 'spacemacs/set-leader-keys-for-major-mode 'evil-leader/set-key-for-mode)
  (evil-leader/set-leader "<SPC>" "M-")
  :config
  (add-hook
   'evil-leader-mode-hook
   (lambda ()
     (when evil-leader/in-all-states
       (let* ((prefixed (read-kbd-macro (concat evil-leader/non-normal-prefix evil-leader/leader)))
              (mode-map (cdr (assoc major-mode evil-leader--mode-maps)))
              (map (or mode-map evil-leader--default-map)))
         (if evil-leader-mode
             (progn
               (define-key evil-visual-state-map prefixed map)
               (define-key evil-motion-state-map prefixed map)
               (define-key evil-insert-state-map prefixed map))
           (progn
             (define-key evil-visual-state-map prefixed nil)
             (define-key evil-motion-state-map prefixed nil)
             (define-key evil-insert-state-map prefixed nil))))))))

(use-package evil
  :straight t
  :init
  (customize-set-variable 'evil-intercept-maps nil)
  (customize-set-variable 'evil-move-cursor-back nil)
  (customize-set-variable 'evil-want-C-u-scroll t)
  (customize-set-variable 'evil-want-Y-yank-to-eol t)
  (customize-set-variable 'evil-symbol-word-search t)
  (customize-set-variable 'evil-kill-on-visual-paste nil)
  (customize-set-variable 'evil-esc-delay 0)
  (customize-set-variable 'evil-shift-width 2)
  (customize-set-variable 'evil-show-paren-range 1)
  (customize-set-variable 'evil-ex-substitute-global t)
  (define-prefix-command 'yq-s-map)
  (setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))
  (setq evil-want-find-undo t)
  (setq evil-insert-state-cursor '(box "green"))
  (defun yq/update-evil-emacs-state-modes (mode-to-remove)
    "remove MODE-TO-REMOVE from evil-emacs-state-modes"
    (setq evil-emacs-state-modes
          (seq-remove
           (lambda (index)
             (eq index mode-to-remove))
           evil-emacs-state-modes)))

  (defun yq/update-evil-insert-state-modes (mode-to-remove)
    "remove MODE-TO-REMOVE from evil-emacs-state-modes"
    (setq evil-insert-state-modes
          (seq-remove
           (lambda (index)
             (eq index mode-to-remove))
           evil-insert-state-modes)))
  :config
  (define-key evil-ex-completion-map (kbd "C-a") #'move-beginning-of-line)
  (define-key evil-ex-completion-map (kbd "C-b") #'backward-char)
  (define-key evil-normal-state-map (kbd "C-b") 'evil-execute-in-emacs-state)
  (define-key evil-normal-state-map "H" #'evil-backward-section-begin)
  (define-key evil-normal-state-map "L" #'evil-forward-section-begin)
  (define-key evil-normal-state-map ">" #'evil-shift-right-line)
  (define-key evil-normal-state-map "<" #'evil-shift-left-line)
  (mapc #'evil-declare-change-repeat
        '(company-complete-common
          company-complete-selection
          company-complete-number
          hippie-expand))
  (define-key evil-visual-state-map ">"
    (lambda nil
      (interactive)
      (progn
        (call-interactively
         'evil-shift-right)
        (execute-kbd-macro "gv"))))

  (define-key evil-visual-state-map "<"
    (lambda nil
      (interactive)
      (progn
        (call-interactively
         'evil-shift-left)
        (execute-kbd-macro "gv"))))
  (define-key evil-visual-state-map "J" (concat ":m '>+1" (kbd "RET") "gv=gv"))
  (define-key evil-visual-state-map "K" (concat ":m '<-2" (kbd "RET") "gv=gv"))
  (define-key evil-insert-state-map (kbd "C-r") 'evil-shift-left-line)
  (define-key evil-insert-state-map (kbd "C-t") 'evil-shift-right-line)
  (spacemacs|define-text-object "$" "dollar" "$" "$")
  (spacemacs|define-text-object "*" "star" "*" "*")
  (spacemacs|define-text-object "8" "block-star" "/*" "*/")
  (spacemacs|define-text-object "|" "bar" "|" "|")
  (spacemacs|define-text-object "%" "percent" "%" "%")
  (spacemacs|define-text-object "/" "slash" "/" "/")
  (spacemacs|define-text-object "_" "underscore" "_" "_")
  (spacemacs|define-text-object "-" "hyphen" "-" "-")
  (spacemacs|define-text-object "~" "tilde" "~" "~")
  (spacemacs|define-text-object "=" "equal" "=" "=")
  (spacemacs|define-text-object "«" "double-angle-bracket" "«" "»")
  (spacemacs|define-text-object "｢" "corner-bracket" "｢" "｣")
  (spacemacs|define-text-object "‘" "single-quotation-mark" "‘" "’")
  (spacemacs|define-text-object "“" "double-quotation-mark" "“" "”")
  (evil-define-text-object evil-pasted (count &rest args)
    (list (save-excursion (evil-goto-mark ?\[) (point))
          (save-excursion (evil-goto-mark ?\]) (point))))
  (define-key evil-inner-text-objects-map "P" 'evil-pasted)
  (evil-define-text-object evil-inner-buffer (count &optional beg end type)
    (list (point-min) (point-max)))
  (define-key evil-inner-text-objects-map "g" 'evil-inner-buffer)
  ;; (define-key evil-evilified-state-map (kbd dotspacemacs-leader-key)
  ;;   spacemacs-default-map)
  (defun yq/duplicate-line ()
    "Duplicate current line."
    (interactive)
    (kill-whole-line)
    (yank)
    (yank))
  (define-key evil-insert-state-map (kbd "C-y") 'yank)
  (define-key evil-normal-state-map (kbd "gy") 'yq/duplicate-line)
  (define-key evil-normal-state-map "gn" 'evil-search-word-forward)
  (define-key evil-normal-state-map "gd" 'spacemacs/jump-to-definition)
  (define-key evil-normal-state-map "gD" 'spacemacs/jump-to-definition-other-window)
  (define-key evil-normal-state-map "j" 'evil-next-visual-line)
  (define-key evil-normal-state-map "k" 'evil-previous-visual-line)
  ;; (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  ;; (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  (define-key evil-normal-state-map "gj" 'evil-next--line)
  (define-key evil-normal-state-map "gk" 'evil-previous-line)
  (define-key evil-normal-state-map "zl" 'hs-hide-level)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-toggle-fold)
  (define-key evil-normal-state-map "s" 'yq-s-map)
  (define-key evil-normal-state-map "t" 'nil)
  (define-key yq-s-map "k" 'yq/kill-this-buffer)
  (define-key yq-s-map "K" 'projectile-kill-buffers)
  (define-key yq-s-map "c" 'yq/delete-window)
  (define-key yq-s-map "h" 'save-buffer)
  (define-key yq-s-map "o" 'dired-jump)
  (define-key evil-normal-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
  (define-key evil-normal-state-map (kbd "C-m") 'evil-jump-item)
  (define-key evil-visual-state-map (kbd "C-m") 'evil-jump-item)
  (define-key evil-visual-state-map (kbd "C-e") 'evil-end-of-line)
  (define-key evil-visual-state-map (kbd "C-a") 'evil-first-non-blank)
  (define-key evil-insert-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
  (define-key evil-insert-state-map (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (define-key evil-insert-state-map (kbd "C-n") 'next-line)
  (define-key evil-insert-state-map (kbd "C-p") 'previous-line)
  (define-key evil-insert-state-map (kbd "C-d") 'delete-forward-char)
  (define-key evil-insert-state-map (kbd "C-m") 'newline-and-indent)
  ;; (define-key evil-insert-state-map (kbd "C-j") 'evil-ret-and-indent)
  (spacemacs/set-leader-keys "TAB" 'spacemacs/alternate-buffer)
  (spacemacs/set-leader-keys "w" nil)
  (spacemacs/set-leader-keys "a" nil)
  (spacemacs/set-leader-keys "t" nil)
  (spacemacs/set-leader-keys "T" nil)
  (spacemacs/set-leader-keys "e" nil)
  (spacemacs/set-leader-keys "b" nil)
  (spacemacs/set-leader-keys "s" nil)
  (spacemacs/set-leader-keys "k" nil)
  (spacemacs/set-leader-keys "x" nil)
  (spacemacs/set-leader-keys "r" nil)
  (spacemacs/set-leader-keys "i" nil)
  (spacemacs/set-leader-keys "f" nil)
  (spacemacs/set-leader-keys "fe" nil)
  (spacemacs/set-leader-keys "h" nil)
  (spacemacs/set-leader-keys "hd" nil)
  (spacemacs/set-leader-keys "tv" 'yq/toggle-visual-line)
  (spacemacs/set-leader-keys "wh" 'evil-window-left)
  (spacemacs/set-leader-keys "wj" 'evil-window-down)
  (spacemacs/set-leader-keys "wk" 'evil-window-up)
  (spacemacs/set-leader-keys "wl" 'evil-window-right)
  (spacemacs/set-leader-keys "rl" 'ivy-resume)
  (spacemacs/set-leader-keys "j" nil)
  (spacemacs/set-leader-keys "j=" 'yq/indent-region-or-buffer)
  (spacemacs/set-leader-keys "fj" 'dired-jump)
  (spacemacs/set-leader-keys "jd" 'dired-jump)
  (spacemacs/set-leader-keys "tD" 'toggle-debug-on-error)
  (spacemacs/set-leader-keys "jD" 'dired-jump-other-window)
  (spacemacs/set-leader-keys "j=" 'yq/indent-region-or-buffer)
  (spacemacs/set-leader-keys "fev" #'view-lossage)
  (evil-define-minor-mode-key 'motion 'visual-line-mode "j" 'evil-next-visual-line)
  (evil-define-minor-mode-key 'motion 'visual-line-mode "k" 'evil-previous-visual-line)
  (spacemacs/set-leader-keys "fed" (lambda () (interactive) (find-file-existing yq-emacs-dotfile-dir)))
  (spacemacs/set-leader-keys "fek" (lambda () (interactive) (find-file-existing "~/.config/karabiner.edn")))
  (load-file "~/.emacs.d/straight/repos/straight.el/straight-x.el")
  (spacemacs/set-leader-keys "feU" #'straight-x-fetch-all)
  (evil-mode 1))

(use-package evil-nerd-commenter
  :straight t
  :commands evilnc-comment-operator
  :init
  (defun spacemacs/copy-and-comment-lines (&optional arg)
    (interactive "p")
    (let ((evilnc-invert-comment-line-by-line nil))
      (evilnc-copy-and-comment-lines arg)))
  (define-key evil-visual-state-map (kbd "C-x C-;") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-normal-state-map "gc" 'evilnc-comment-operator)
  (define-key evil-normal-state-map "gc" 'evilnc-comment-operator)
  (define-key evil-normal-state-map "gY" 'spacemacs/copy-and-comment-lines))

(use-package evil-snipe
  :straight t
  :diminish evil-snipe-mode
  :diminish evil-snipe-override-mode
  :diminish evil-snipe-local-mode
  :init
  (setq evil-snipe-scope 'whole-buffer)
  (setq evil-snipe-enable-highlight t)
  (setq evil-snipe-enable-incremental-highlight t)
  (setq evil-snipe-auto-disable-substitute t)
  (setq evil-snipe-show-prompt nil)
  (setq evil-snipe-smart-case t)
  :config
  ;; remap s
  ;; use t as evil-snipe-s in normal mode
  (evil-define-key* '(normal motion) evil-snipe-local-mode-map
                    "s" nil
                    "S" nil
                    ;; "t" #'evil-snipe-s
                    ;; "T" #'evil-snipe-S
                    "t" nil
                    "T" nil)
  (yq/add-toggle evil-snipe :mode evil-snipe-mode)
  ;; (add-hook 'org-mode-hook 'yq/toggle-evil-snipe-off)
  (setq evil-snipe-auto-disable-substitute nil)
  (evil-snipe-mode 1)
  (setq evil-snipe-repeat-scope 'whole-buffer)
  (evil-snipe-override-mode 1))

(use-package evil-surround
  :straight t
  :init
  (global-evil-surround-mode 1)
  :config
  (setq evil-surround-pairs-alist '((40 "(" . ")")
                                    (91 "[" . "]")
                                    (123 "{" . "}")
                                    (41 "(" . ")")
                                    (93 "[" . "]")
                                    (125 "{" . "}")
                                    (35 "#{" . "}")
                                    (98 "(" . ")")
                                    (66 "{" . "}")
                                    (62 "<" . ">")
                                    (116 . evil-surround-read-tag)
                                    (60 . evil-surround-read-tag)
                                    (102 . evil-surround-function)))
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute))

(straight-use-package 'embrace)
(use-package evil-embrace
  :straight t
  :init
  (evil-embrace-enable-evil-surround-integration)
  :config
  (define-key evil-normal-state-map "gs" 'embrace-commander))

(use-package evil-mc
  :straight t
  :diminish evil-mc-mode
  :commands (evil-mc-make-cursor-move-next-line
             evil-mc-make-cursor-move-prev-line
             evil-mc-mode
             evil-mc-undo-all-cursors
             global-evil-mc-mode)
  :init
  (setq evil-mc-key-map '())
  (add-hook 'after-init-hook #'global-evil-mc-mode)
  (setq evil-mc-one-cursor-show-mode-line-text nil)
  (advice-add 'keyboard-quit :before #'evil-mc-undo-all-cursors)
  (define-key evil-normal-state-map (kbd "M-j") 'evil-mc-make-cursor-move-next-line)
  (define-key evil-normal-state-map (kbd "M-k") 'evil-mc-make-cursor-move-prev-line)
  (define-key evil-normal-state-map (kbd "M-n") 'evil-mc-make-and-goto-next-match)
  (define-key evil-normal-state-map (kbd "M-p") 'evil-mc-make-and-goto-prev-match)
  :config
  (add-to-list 'evil-mc-known-commands '(mwim-beginning-of-code-or-line (:default . evil-first-non-blank)))
  (add-to-list 'evil-mc-known-commands '(mwim-end-of-code-or-line (:default . evil-end-of-line))))

(use-package evil-multiedit
  :straight t
  :config
  (define-key evil-visual-state-map "v" #'evil-multiedit-match-all)
  (define-key evil-insert-state-map (kbd "M-n") #'evil-multiedit-match-and-next)
  (define-key evil-insert-state-map (kbd "M-p") #'evil-multiedit-match-and-prev)
  (define-key evil-normal-state-map (kbd "C-n") #'evil-multiedit-match-and-next)
  (define-key evil-normal-state-map (kbd "C-p") #'evil-multiedit-match-and-prev)
  (define-key evil-visual-state-map (kbd "C-n") #'evil-multiedit-match-and-next)
  (define-key evil-visual-state-map (kbd "C-p") #'evil-multiedit-match-and-prev)
  (define-key evil-multiedit-state-map (kbd "C-n") #'evil-multiedit-match-and-next)
  (define-key evil-multiedit-state-map (kbd "C-p") #'evil-multiedit-match-and-prev)
  (define-key evil-multiedit-state-map (kbd "n") #'evil-multiedit-next)
  (define-key evil-multiedit-state-map (kbd "N") #'evil-multiedit-prev)
  (define-key evil-multiedit-insert-state-map (kbd "C-n") #'evil-multiedit-match-and-next)
  (define-key evil-multiedit-insert-state-map (kbd "C-p") #'evil-multiedit-match-and-prev)
  (define-key evil-motion-state-map (kbd "RET") #'evil-multiedit-toggle-or-restrict-region)
  (evil-ex-define-cmd "ie[dit]" #'evil-multiedit-ex-match))

(use-package evil-args
  :straight t
  :config ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

(use-package evil-search-highlight-persist
  :straight t
  :diminish global-highlight-parentheses-mode
  :init (setq evil-search-highlight-persist-all-windows t)
  :config
  (set-face-attribute 'evil-search-highlight-persist-highlight-face nil
                      :inherit 'lazy-highlight
                      :background nil
                      :foreground nil)
  (global-evil-search-highlight-persist t)
  (setq evil-search-highlight-string-min-len 1)
  evil-search-highlight-persist-all-windows t)

(use-package evil-textobj-anyblock
  :straight t
  :after evil
  :config
  (define-key evil-inner-text-objects-map "f" 'evil-textobj-anyblock-inner-block)
  (define-key evil-outer-text-objects-map "f" 'evil-textobj-anyblock-a-block))

(use-package evil-indent-plus
  :straight t
  :after evil
  :init (evil-indent-plus-default-bindings))

(use-package evil-matchit
  :straight t
  :init
  (add-hook 'after-init-hook #'global-evil-matchit-mode)
  :config
  ;; (define-key evil-normal-state-map (kbd "C-;") 'evilmi-select-items)
  (define-key evil-visual-state-map (kbd "C-m") 'evilmi-jump-items)
  (define-key evil-normal-state-map (kbd "C-m") 'evilmi-jump-items))

(use-package anzu
  :straight t
  :diminish anzu-mode
  :after evil
  :init
  (global-anzu-mode t)
  (setq anzu-cons-mode-line-p nil
        anzu-minimum-input-length 1
        anzu-search-threshold 250)
  :config
  (evil-set-command-property 'evilmi-jump-items :jump t)
  ;; Avoid anzu conflicts across buffers
  (mapc #'make-variable-buffer-local
        '(anzu--total-matched
          anzu--current-position anzu--state
          anzu--cached-count anzu--cached-positions anzu--last-command
          anzu--last-isearch-string anzu--overflow-p))
  (setcar (cdr (assq 'isearch-mode minor-mode-alist))
          '(:eval (anzu--update-mode-line)))
  ;; Ensure anzu state is cleared when searches & iedit are done
  (add-hook 'isearch-mode-end-hook #'anzu--reset-status t)
  (add-hook 'iedit-mode-end-hook #'anzu--reset-status))

(use-package evil-anzu
  :straight t
  :diminish anzu-mode)

(straight-use-package 'bind-map)
(yq/get-modules "evil-evilified-state.el")
(evilified-state-evilify-map occur-mode-map
  :mode occur-mode)


(use-package evil-numbers
  :straight t
  :init
  (global-set-key (kbd "C-c =") #'evil-numbers/inc-at-pt)
  (global-set-key (kbd "C-c -") #'evil-numbers/dec-at-pt))

(use-package evil-textobj-syntax
  :straight (:host github :repo "laishulu/evil-textobj-syntax"))

(load-file "./terminal-keybindings.el")