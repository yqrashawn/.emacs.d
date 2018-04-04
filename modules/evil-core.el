(yq/get-modules "evil-core-funcs.el")
(yq/get-modules "core-jump.el")
;; (yq/get-modules "core-keybindings.el")
(global-set-key (kbd "C-g") 'keyboard-quit)
(global-set-key (kbd "C-x 1") 'spacemacs/toggle-maximize-buffer)

(use-package undo-tree
  :straight (:host github :repo "emacsmirror/undo-tree")
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (global-set-key (kbd "s-y") 'undo-tree-redo))

(use-package goto-chg
  :straight (:host github :repo "emacs-evil/goto-chg"))

(use-package evil-leader
  :straight t
  :init
  (setq evil-leader/in-all-states t)
  :config
  (defalias 'spacemacs/set-leader-keys 'evil-leader/set-key)
  (defalias 'spacemacs/set-leader-keys-for-major-mode 'evil-leader/set-key-for-mode)
  (evil-leader/set-leader "<SPC>" "M-")
  (global-evil-leader-mode))

(use-package evil
  :straight t
  :init
  (customize-set-variable 'evil-move-cursor-back nil)
  (customize-set-variable 'evil-want-C-u-scroll t)
  (customize-set-variable 'evil-want-Y-yank-to-eol t)
  (customize-set-variable 'evil-symbol-word-search t)
  (customize-set-variable 'evil-kill-on-visual-paste nil)
  (customize-set-variable 'evil-esc-delay 0)
  (customize-set-variable 'evil-shift-width 2)
  (customize-set-variable 'evil-show-paren-range 1)
  :config
  (mapc #'evil-declare-change-repeat
        '(company-complete-common
          company-complete-selection
          company-complete-number
          hippie-expand))
  (defmacro evil-map (state key seq)
    "Map for a given STATE a KEY to a sequence SEQ of keys.

Can handle recursive definition only if KEY is the first key of SEQ.
Example: (evil-map visual \"<\" \"<gv\")"
    (let ((map (intern (format "evil-%S-state-map" state))))
      `(define-key ,map ,key
         (lambda ()
           (interactive)
           ,(if (string-equal key (substring seq 0 1))
                `(progn
                   (call-interactively ',(lookup-key evil-normal-state-map key))
                   (execute-kbd-macro ,(substring seq 1)))
              (execute-kbd-macro ,seq))))))
  (evil-map visual "<" "<gv")
  (evil-map visual ">" ">gv")
  (define-key evil-visual-state-map "J" (concat ":m '>+1" (kbd "RET") "gv=gv"))
  (define-key evil-visual-state-map "K" (concat ":m '<-2" (kbd "RET") "gv=gv"))
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
  (define-key evil-normal-state-map "gj" 'evil-next-visual-line)
  (define-key evil-normal-state-map "gk" 'evil-previous-visual-line)
  (define-key evil-normal-state-map "zl" 'hs-hide-level)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-toggle-fold)
  (define-key evil-normal-state-map "s" nil)
  (define-key evil-normal-state-map "sk" 'yq/kill-this-buffer)
  (define-key evil-normal-state-map "sc" 'yq/delete-window)
  (define-key evil-normal-state-map "sh" 'save-buffer)
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
  (define-key evil-insert-state-map (kbd "C-j") 'evil-ret-and-indent)
  (spacemacs/set-leader-keys "TAB" 'spacemacs/alternate-buffer)
  (spacemacs/set-leader-keys "w" nil)
  (spacemacs/set-leader-keys "a" nil)
  (spacemacs/set-leader-keys "t" nil)
  (spacemacs/set-leader-keys "T" nil)
  (spacemacs/set-leader-keys "e" nil)
  (spacemacs/set-leader-keys "b" nil)
  (spacemacs/set-leader-keys "r" nil)
  (spacemacs/set-leader-keys "i" nil)
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
  (evil-define-minor-mode-key 'motion 'visual-line-mode "j" 'evil-next-visual-line)
  (evil-define-minor-mode-key 'motion 'visual-line-mode "k" 'evil-previous-visual-line)
  (evil-mode 1))

(use-package evil-nerd-commenter
  :straight t
  :commands evilnc-comment-operator
  :init
  (progn
    ;; double all the commenting functions so that the inverse operations
    ;; can be called without setting a flag
    (defun spacemacs/comment-or-uncomment-lines-inverse (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line t))
        (evilnc-comment-or-uncomment-lines arg)))

    (defun spacemacs/comment-or-uncomment-lines (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line nil))
        (evilnc-comment-or-uncomment-lines arg)))

    (defun spacemacs/copy-and-comment-lines-inverse (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line t))
        (evilnc-copy-and-comment-lines arg)))

    (defun spacemacs/copy-and-comment-lines (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line nil))
        (evilnc-copy-and-comment-lines arg)))

    (defun spacemacs/quick-comment-or-uncomment-to-the-line-inverse
        (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line t))
        (evilnc-comment-or-uncomment-to-the-line arg)))

    (defun spacemacs/quick-comment-or-uncomment-to-the-line (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line nil))
        (evilnc-comment-or-uncomment-to-the-line arg)))

    (defun spacemacs/comment-or-uncomment-paragraphs-inverse (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line t))
        (evilnc-comment-or-uncomment-paragraphs arg)))

    (defun spacemacs/comment-or-uncomment-paragraphs (&optional arg)
      (interactive "p")
      (let ((evilnc-invert-comment-line-by-line nil))
        (evilnc-comment-or-uncomment-paragraphs arg)))
    (define-key evil-visual-state-map (kbd "C-x C-;") 'evilnc-comment-or-uncomment-lines)
    (define-key evil-normal-state-map "gc" 'evilnc-comment-operator)
    (define-key evil-normal-state-map "gY" 'spacemacs/copy-and-comment-lines)
    (spacemacs/set-leader-keys
      ";"  'evilnc-comment-operator
      "cl" 'spacemacs/comment-or-uncomment-lines
      "cL" 'spacemacs/comment-or-uncomment-lines-inverse
      "cp" 'spacemacs/comment-or-uncomment-paragraphs
      "cP" 'spacemacs/comment-or-uncomment-paragraphs-inverse
      "ct" 'spacemacs/quick-comment-or-uncomment-to-the-line
      "cT" 'spacemacs/quick-comment-or-uncomment-to-the-line-inverse
      "cy" 'spacemacs/copy-and-comment-lines
      "cY" 'spacemacs/copy-and-comment-lines-inverse)))

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
                    "t" #'evil-snipe-s
                    "T" #'evil-snipe-S)
  (yq/add-toggle evil-snipe :mode evil-snipe-mode)
  (add-hook 'org-mode-hook 'yq/toggle-evil-snipe-off)
  (setq evil-snipe-auto-disable-substitute nil)
  (evil-snipe-mode 1)
  (setq evil-snipe-repeat-scope 'whole-buffer)
  (evil-snipe-override-mode 1))

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1)
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute))

(use-package evil-args
  :straight t
  :config ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

(use-package evil-search-highlight-persist
  :straight t
  :diminish global-highlight-parentheses-mode
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
  :config
  (define-key evil-inner-text-objects-map "f" 'evil-textobj-anyblock-inner-block)
  (define-key evil-outer-text-objects-map "f" 'evil-textobj-anyblock-a-block))

(use-package evil-mc
  :straight t
  :diminish evil-mc-mode
  :init
  (setq evil-mc-one-cursor-show-mode-line-text nil)
  (global-evil-mc-mode t)
  :init (add-hook 'after-init-hook #'global-evil-mc-mode)
  :config
  ;; (defun yq/evil-mc-keyboard-quit (fn &rest props)
  ;;   "docstring"
  ;;   (interactive "P")
  ;;   (if (and evil-mc-mode evil-mc-cursor-list)
  ;;       (funcall 'evil-mc-undo-all-cursors)
  ;;     (apply fn props)))
  ;; (advice-add 'keyboard-quit :around #'yq/evil-mc-keyboard-quit)
  (advice-add 'keyboard-quit :before #'evil-mc-undo-all-cursors)
  (setq evil-mc-mode-line-text-cursor-color t)
  (define-key evil-normal-state-map (kbd "C-n") 'evil-mc-make-and-goto-next-match)
  (define-key evil-normal-state-map (kbd "C-p") 'evil-mc-make-and-goto-prev-match)
  (define-key evil-normal-state-map (kbd "C-S-n") 'evil-mc-skip-and-goto-next-match)
  (define-key evil-normal-state-map (kbd "M-j") 'evil-mc-make-cursor-move-next-line)
  (define-key evil-normal-state-map (kbd "M-k") 'evil-mc-make-cursor-move-prev-line)
  (define-key evil-normal-state-map (kbd "<C-return>") 'evil-mc-make-all-cursors))

(use-package evil-matchit
  :straight t
  :init
  (add-hook 'after-init-hook #'global-evil-matchit-mode)
  :config
  (define-key evil-normal-state-map (kbd "C-;") 'evilmi-select-items)
  (define-key evil-normal-state-map (kbd "C-m") 'evilmi-jump-items))

(use-package anzu
  :straight t
  :diminish anzu-mode
  :init
  (global-anzu-mode t)
  :config
  (defun yq/anzu-update-func (here total)
    (when anzu--state
      (let ((status (cl-case anzu--state
                      (search (format "<%d/%d>" here total))
                      (replace-query (format "(%d Replaces)" total))
                      (replace (format "<%d/%d>" here total)))))
        (propertize status 'face 'anzu-mode-line))))

  (custom-set-variables
   '(anzu-mode-line-update-function #'yq/anzu-update-func))
  (setq anzu-cons-mode-line-p nil)
  (setcar (cdr (assq 'isearch-mode minor-mode-alist))
          '(:eval (anzu--update-mode-line))))

(use-package evil-anzu
  :straight t
  :diminish anzu-mode
  :init
  (global-anzu-mode t)
  :config
  (setq anzu-search-threshold 1000)
  (setq anzu-cons-mode-line-p nil))

(straight-use-package 'bind-map)
(yq/get-modules "evil-evilified-state.el")
(evilified-state-evilify-map occur-mode-map
  :mode occur-mode)
