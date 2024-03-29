;;; navigation.el --- configs about navigation -*- lexical-binding: t; -*-

(setq yq-quick-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?\; ?q ?w ?e ?r
                         ?t ?y ?u ?i ?o ?p ?z ?x ?c ?v ?b ?n ?m
                         ?A ?S ?D ?F ?G ?H ?J ?K ?L ?Q ?W ?E ?R
                         ?T ?Y ?U ?I ?O ?P ?Z ?X ?C ?V ?B ?N ?M))

(yq/get-modules "counsel-funcs.el")

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless partial-completion)))

(use-package ivy
  :straight (:host github :repo "abo-abo/swiper" :branch "master"
             :files
             (:defaults (:exclude "swiper.el" "counsel.el" "ivy-hydra.el") "doc/ivy-help.org")
             :upstream (:host github :repo "abo-abo/swiper"))
  :diminish ivy-mode
  :defer t
  :custom
  (ivy-use-selectable-prompt t)
  (ivy-magic-tilde nil)
  (ivy-height 16)
  (ivy-use-virtual-buffers t)
  :init
  (setq-default ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
  (define-key yq-s-map "b" 'ivy-switch-buffer)
  ;; http://pragmaticemacs.com/emacs/open-a-recent-directory-in-dired-revisited/
  (defun bjm/ivy-dired-recent-dirs ()
    "Present a list of recently used directories and open the selected one in dired"
    (interactive)
    (let ((recent-dirs (delete-dups
                        (mapcar
                         (lambda (file)
                           (if (not (string-match
                                     "/\\(rsh\\|ssh\\|telnet\\|su\\|sudo\\|sshx\\|krlogin\\|ksu\\|rcp\\|scp\\|rsync\\|scpx\\|fcp\\|nc\\|ftp\\|smb\\|adb\\):"
                                     file))
                               (if (file-directory-p file)
                                   file
                                 (file-name-directory file))
                             (if (file-name-directory file)
                                 (file-name-directory file))))
                         recentf-list))))
      (let ((dir (ivy-read
                  "Directory: "
                  recent-dirs
                  ;; :re-builder #'ivy--regex
                  :sort nil
                  :initial-input nil)))
        (dired dir))))

  (define-key yq-s-map "y" 'bjm/ivy-dired-recent-dirs)

  (defun +ivy-switch-buffer-next-line ()
    (interactive)
    (if (minibufferp) (ivy-next-line)
      (ivy-switch-buffer)))
  (defun +ivy-switch-buffer-prev-line ()
    (interactive)
    (if (minibufferp) (ivy-previous-line)
      (ivy-switch-buffer)))

  ;; (global-set-key (kbd "C-x C-8 l") 'ivy-alt-done)
  (global-set-key (kbd "C-x C-9 j") '+ivy-switch-buffer-next-line)
  (global-set-key (kbd "C-x C-9 k") '+ivy-switch-buffer-prev-line)
  ;; (global-set-key (kbd "C-M-S-s-j") '+ivy-switch-buffer-next-line)
  ;; (global-set-key (kbd "C-M-S-s-k") '+ivy-switch-buffer-prev-line)
  ;; (global-set-key (kbd "C-x C-8 a" ) 'ivy-beginning-of-buffer)
  ;; (global-set-key (kbd "C-x C-8 e" ) 'ivy-end-of-buffer)
  ;; (global-set-key (kbd "C-x C-8 u" ) 'ivy-scroll-down-command)
  ;; (global-set-key (kbd "C-x C-8 d" ) 'ivy-scroll-up-command)
  (ivy-mode 1)
  :config
  (setenv "FZF_DEFAULT_COMMAND" "rg --files --no-ignore --hidden --follow -g \"!{.git,node_modules}/*\" 2> /dev/null")
  (with-eval-after-load 'counsel
    (ivy-set-actions 'counsel-recentf spacemacs--ivy-file-actions))
  ;; docs: https://oremacs.com/swiper/#completion-styles

  (add-to-list 'ivy-re-builders-alist '(forge-create-pullreq . ivy--regex-fuzzy))
  (add-to-list 'ivy-re-builders-alist '(counsel-git . ivy--regex-fuzzy))
  (add-to-list 'ivy-re-builders-alist '(projector-run-command-buffer-prompt . ivy--regex-fuzzy))
  ;; (add-to-list 'ivy-re-builders-alist '(spacemacs/counsel-search . spacemacs/ivy--regex-plus))
  ;; (add-to-list 'ivy-re-builders-alist '(spacemacs/search-auto . spacemacs/ivy--regex-plus))
  (add-to-list 'ivy-re-builders-alist '(spacemacs/counsel-search . ivy--regex-plus))
  (add-to-list 'ivy-re-builders-alist '(spacemacs/search-auto . ivy--regex-plus))

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

  (evil-define-key 'normal ivy-occur-mode-map "w" 'ivy-wgrep-change-to-wgrep-mode)
  (evil-define-key 'normal ivy-occur-mode-map "s" 'wgrep-save-all-buffers)
  (ivy-set-occur 'spacemacs/counsel-search 'spacemacs//counsel-occur)
  (evil-set-initial-state 'ivy-occur-grep-mode 'normal)
  (evil-make-overriding-map ivy-occur-mode-map 'normal)
  (ido-mode -1)

  ;; (defun yq-ivy-format-function (cands)
  ;;   "Transform CANDS into a string for minibuffer."
  ;;   (print cands)
  ;;   (let ((cands (--map-indexed (format "%s %s" (char-to-string (elt yq-quick-keys it-index)) it) cands)))
  ;;     (ivy--format-function-generic
  ;;      (lambda (str)
  ;;        (ivy--add-face str 'ivy-current-match))
  ;;      #'identity
  ;;      cands
  ;;      "\n")))
  ;; (setq ivy-format-functions-alist '((t . yq-ivy-format-function)))
  ;; (setq ivy-format-function 'yq-ivy-format-function)
  ;; (defun +ivy-select-index (&optional key)
  ;;   (interactive)
  ;;   (let ((key (or key (read-char "key: " t))))
  ;;     (ivy-next-line (seq-position yq-quick-keys key))
  ;;     (ivy--exhibit)
  ;;     (ivy-alt-done)))
  ;; (define-key ivy-minibuffer-map (kbd "C-x C-6 1") (lambda () (interactive) (+ivy-select-index ?a)))
  ;; (define-key ivy-minibuffer-map (kbd "C-x C-6 2") (lambda () (interactive) (+ivy-select-index ?s)))
  ;; (define-key ivy-minibuffer-map (kbd "C-x C-6 3") (lambda () (interactive) (+ivy-select-index ?d)))
  ;; (define-key ivy-minibuffer-map (kbd "C-x C-6 4") (lambda () (interactive) (+ivy-select-index ?f)))
  ;; (define-key ivy-minibuffer-map (kbd "C-x C-6 5") (lambda () (interactive) (+ivy-select-index ?g)))
  ;; (define-key ivy-minibuffer-map (kbd "C-x C-6 6") (lambda () (interactive) (+ivy-select-index ?h)))
  ;; (define-key ivy-minibuffer-map (kbd "C-x C-6 7") (lambda () (interactive) (+ivy-select-index ?j)))
  ;; (define-key ivy-minibuffer-map (kbd "C-x C-6 8") (lambda () (interactive) (+ivy-select-index ?k)))
  ;; (define-key ivy-minibuffer-map (kbd "C-x C-6 9") (lambda () (interactive) (+ivy-select-index ?l)))
  ;; (define-key ivy-minibuffer-map (kbd "C-x C-6 0") (lambda () (interactive) (+ivy-select-index 59)))

  (defun prot/counsel-fzf-dir (arg)
    "Specify root directory for `counsel-fzf'."
    (+counsel-fzf-rg-files ivy-text
                           (read-directory-name
                            (concat (car (split-string counsel-fzf-cmd))
                                    " in directory: "))))

  (defun prot/counsel-rg-dir (arg)
    "Specify root directory for `counsel-rg'."
    (let ((current-prefix-arg '(4)))
      (counsel-rg ivy-text nil "")))

  ;; Pass functions as appropriate Ivy actions (accessed via M-o)
  (ivy-add-actions
   'counsel-fzf
   '(("r" prot/counsel-fzf-dir "change root directory")
     ("g" prot/counsel-rg-dir "use ripgrep in root directory")))

  (ivy-add-actions
   'counsel-rg
   '(("r" prot/counsel-rg-dir "change root directory")
     ("z" prot/counsel-fzf-dir "find file with fzf in root directory")))

  (ivy-add-actions
   'counsel-find-file
   '(("g" prot/counsel-rg-dir "use ripgrep in root directory")
     ("z" prot/counsel-fzf-dir "find file with fzf in root directory")))



  (use-package ivy-hydra
    :straight t
    :after ivy
    :defer t
    :config
    (defun yq/ivy-call-kill-buffer-action ()
      "Call the current action without exiting completion."
      (interactive)
      (let ((action 'ivy--kill-buffer-action))
        (when action
          (let* ((collection (ivy-state-collection ivy-last))
                 (x (cond
                     ;; Alist type.
                     ((and (consp collection)
                           (consp (car collection))
                           ;; Previously, the cdr of the selected
                           ;; candidate would be returned.  Now, the
                           ;; whole candidate is returned.
                           (let (idx)
                             (if (setq idx (get-text-property
                                            0 'idx (ivy-state-current ivy-last)))
                                 (nth idx collection)
                               (assoc (ivy-state-current ivy-last)
                                      collection)))))
                     (ivy--directory
                      (expand-file-name
                       (ivy-state-current ivy-last)
                       ivy--directory))
                     ((equal (ivy-state-current ivy-last) "")
                      ivy-text)
                     (t
                      (ivy-state-current ivy-last)))))
            (if (eq action 'identity)
                (funcall action x)
              (select-window (ivy--get-window ivy-last))
              (set-buffer (ivy-state-buffer ivy-last))
              (prog1 (with-current-buffer (ivy-state-buffer ivy-last)
                       (unwind-protect (funcall action x)
                         (ivy-recursive-restore)))
                (unless (or (eq ivy-exit 'done)
                            (equal (selected-window)
                                   (active-minibuffer-window))
                            (null (active-minibuffer-window)))
                  (select-window (active-minibuffer-window)))))))))

    (defhydra hydra-ivy (:hint nil
                         :color pink)
      "
^ ^ ^ ^ ^ ^ | ^Call^      ^ ^  | ^Cancel^ | ^Options^ | Action _w_/_s_/_a_: %-14s(ivy-action-name)
^-^-^-^-^-^-+-^-^---------^-^--+-^-^------+-^-^-------+-^^^^^^^^^^^^^^^^^^^^^^^^^^^^^---------------------------
^ ^ _k_ ^ ^ | _f_ollow _o_ccur | _i_nsert | _c_: calling %-5s(if ivy-calling \"on\" \"off\") _C_ase-fold: %-10`ivy-case-fold-search
_h_ ^+^ _l_ | _d_one      ^ ^  |          | _m_: matcher %-5s(ivy--matcher-desc)^^^^^^^^^^^^ _t_runcate: %-11`truncate-lines
^ ^ _j_ ^ ^ | _u_p _d_own      |          | _<_/_>_: shrink/grow _q_uit _x_ kill buffer^^ _D_efinition of this menu
"
      ;; arrows
      ("h" spacemacs/counsel-up-directory-no-error)
      ("j" ivy-next-line)
      ("k" ivy-previous-line)
      ("l" ivy-end-of-buffer)
      ("u" ivy-scroll-down-command)
      ("d" ivy-scroll-up-command)
      ;; actions
      ("C-g" keyboard-escape-quit :exit t)
      ("i" nil)
      ("C-o" nil)
      ("f" ivy-alt-done :exit nil)
      ("C-j" ivy-alt-done :exit nil)
      ("g" ivy-beginning-of-buffer)
      ("G" ivy-end-of-buffer)
      ("C-m" ivy-done :exit t)
      ("c" ivy-toggle-calling)
      ("m" ivy-rotate-preferred-builders)
      (">" ivy-minibuffer-grow)
      ("<" ivy-minibuffer-shrink)
      ("w" ivy-prev-action)
      ("s" ivy-next-action)
      ("a" ivy-read-action)
      ("x" yq/ivy-call-kill-buffer-action)
      ("t" (setq truncate-lines (not truncate-lines)))
      ("C" ivy-toggle-case-fold)
      ("o" ivy-occur :exit t)
      ("q" (ivy-exit-with-action (lambda (_))) :exit t)
      ("D" (ivy-exit-with-action
            (lambda (_) (find-function 'hydra-ivy/body)))
       :exit t))))

(use-package swiper
  :straight (:host github :repo "abo-abo/swiper" :branch "master"
             :files ("swiper.el")
             :upstream (:host github :repo "abo-abo/swiper"))
  :defer t
  :bind
  ("C-s" . #'swiper-isearch)
  ("C-c s" . #'swiper-isearch-thing-at-point)
  ("C-c C-s" . #'swiper-thing-at-point)
  :init
  ;; (define-key yq-s-map (kbd "n") 'spacemacs/swiper-all-region-or-symbol)
  (global-set-key (kbd "C-SPC") #'counsel-grep-or-swiper)
  (global-set-key (kbd "^@") #'counsel-grep-or-swiper)
  (global-set-key (kbd "C-s") #'swiper-isearch)
  (global-set-key (kbd "C-S-SPC") 'spacemacs/swiper-region-or-symbol)
  (define-key evil-normal-state-map "gN" 'spacemacs/swiper-region-or-symbol)
  (spacemacs/set-leader-keys "fes" (lambda () (interactive) (find-file-existing "~/.ssh/config.gpg") (swiper))))

(use-package counsel
  :straight (:host github :repo "abo-abo/swiper" :branch "master"
                   :files ("counsel.el")
                   :upstream (:host github :repo "abo-abo/swiper"))
  :defer t
  :diminish counsel-mode
  :bind
  ("C-c U" . #'counsel-unicode-char)
  ("C-c C-y" . #'counsel-yank-pop)
  :init
  (spacemacs/set-leader-keys "<SPC>" #'counsel-M-x)
  (spacemacs/set-leader-keys "ff" #'counsel-find-file)
  (spacemacs/set-leader-keys "fr" #'counsel-recentf)
  (spacemacs/set-leader-keys "fF" 'find-file-other-window)
  (spacemacs/set-leader-keys "hk" #'counsel-descbinds)
  (spacemacs/set-leader-keys "hf" #'counsel-describe-function)
  (spacemacs/set-leader-keys "hdf" #'counsel-describe-face)
  (spacemacs/set-leader-keys "hdv" #'counsel-describe-variable)
  (spacemacs/set-leader-keys "hdk" #'describe-key)
  (spacemacs/set-leader-keys "hdK" 'describe-keymap)
  (spacemacs/set-leader-keys "hdl" #'view-lossage)
  (spacemacs/set-leader-keys "fJ" 'spacemacs/open-junk-file)
  (spacemacs/set-leader-keys "fel" 'counsel-find-library)
  (spacemacs/set-leader-keys "fw" nil)
  (spacemacs/set-leader-keys "fww" (defl (counsel-find-file "~/workspace/")))
  (spacemacs/set-leader-keys "fwt" (defl (counsel-find-file "~/workspace/third")))
  (spacemacs/set-leader-keys "fwh" (defl (counsel-find-file "~/workspace/home")))
  (spacemacs/set-leader-keys "fwo" (defl (counsel-find-file "~/workspace/office")))
  (define-key yq-s-map "f" #'counsel-rg)
  (define-key yq-s-map "l" 'spacemacs/counsel-jump-in-buffer)
  (define-key yq-s-map "j" #'counsel-recentf)
  (global-set-key (kbd "C-x C-r") #'counsel-recentf)
  (define-key yq-s-map (kbd "SPC") 'counsel-M-x)
  (define-key evil-normal-state-map (kbd "M-y") 'counsel-yank-pop)
  :config
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never %s %s")
  (and (fboundp 'counsel--elisp-to-pcre) (defalias 'counsel-unquote-regex-parens 'counsel--elisp-to-pcre))
  (defun counsel-imenu-comments ()
    "Imenu display comments."
    (interactive)
    (let* ((imenu-create-index-function 'evilnc-imenu-create-index-function))
      (counsel-imenu)))
  (counsel-mode 1)
  (setq counsel-find-file-occur-cmd "ls | grep -i -E '%s' | gxargs -d '\n' ls")
  (define-key ivy-minibuffer-map (kbd "C-c C-e") 'spacemacs//counsel-edit)
  (define-key ivy-minibuffer-map (kbd "C-n") 'ivy-next-history-element)
  (define-key ivy-minibuffer-map (kbd "C-p") 'ivy-previous-history-element)
  (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
  (define-key ivy-switch-buffer-map (kbd "C-k") 'ivy-previous-line)
  (define-key ivy-switch-buffer-map (kbd "C-d") 'ivy-switch-buffer-kill)
  (define-key ivy-minibuffer-map (kbd "C-w") 'backward-kill-word)
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file))

(use-package counsel-tramp
  :straight t
  :commands (counsel-tramp)
  :init
  ;; if zsh still failed
  ;; https://www.emacswiki.org/emacs/TrampMode#toc7
  ;; (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
  (spacemacs/set-leader-keys "fT" 'counsel-tramp))

(use-package prescient
  :disabled
  :straight t)

(use-package ivy-prescient
  :straight t
  :disabled
  :after (prescient ivy)
  :init
  (ivy-prescient-mode))

(use-feature imenu
  :defer t
  :config
  (defun imenu-use-feature ()
    (add-to-list
     'imenu-generic-expression
     '("Features" "^\\s-*(\\(use-feature\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 2)))
  (add-hook 'emacs-lisp-mode-hook #'imenu-use-feature))

(use-package imenu-anywhere
  :straight t
  :commands (imenu-anywhere)
  :init (define-key yq-s-map "L" 'imenu-anywhere))

(use-package helpful
  :straight t
  :commands (helpful-variable helpful-key helpful-function)
  :init
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)
  (global-set-key [remap describe-symbol]   #'helpful-symbol)
  (spacemacs/set-leader-keys "hdV" 'helpful-variable)
  (spacemacs/set-leader-keys "hdv" 'describe-variable)
  (spacemacs/set-leader-keys "hdk" 'helpful-key)
  (spacemacs/set-leader-keys "hF" 'helpful-function)
  (evil-define-key 'normal helpful-mode-map "Q"
    (lambda ()
      (interactive)
      (quit-window)
      (yq/delete-window)))
  (defun yq/kill-buffer-and-window ()
    (interactive)
    (kill-current-buffer)
    (yq/delete-window))
  (evil-define-key 'normal helpful-mode-map "q" 'quit-window))

(use-package wgrep
  :straight t
  :defer t
  :init
  (setq wgrep-auto-save-buffer t))

(use-package smex
  :straight t
  :defer t
  :init
  (setq-default smex-history-length 128)
  (setq-default smex-save-file (concat yq-emacs-cache-dir ".smex-items")))

(use-package projectile
  :straight t
  :defer t
  :hook (after-init . projectile-mode)
  :diminish projectile-mode
  :init
  ;(setq projectile-project-search-path '("~/workspace/" "~/.emacs.d/straight/repos/"))
  (setq projectile-completion-system 'ivy)
  :config
  (defun yank-file-relative-path ()
    (interactive)
    (let ((p (file-relative-name (buffer-file-name) (projectile-project-root))))
      (if p
          (progn (kill-new p)
                 (message "\"%s\" into kill ring" p))
        (message "not in a project"))))
  (spacemacs/set-leader-keys "kf" 'yank-file-relative-path)
  ;; (define-key yq-s-map "m" #'projectile-find-file-dwim)
  (when (executable-find "fd")
    (setq projectile-git-command "fd . -t f -0"
          projectile-generic-command projectile-git-command))
  (setq projectile-globally-ignored-directories
        (append projectile-globally-ignored-directories
                '("node_modules" "build" "tests" ".cache")))

  (setq projectile-project-root-files
        (append projectile-project-root-files
                '(".tabnine_root")))
  (setq projectile-sort-order 'recently-active)
  (setq projectile-globally-ignored-file-suffixes '(".elc" ".min.js" ".min.css" ".unmin.js" ".unmin.css" ".map"))
  (setq projectile-verbose nil)
  (setq projectile-enable-idle-timer t)
  (setq projectile-idle-timer-seconds 300)
  (setq projectile-idle-timer-hook '(projectile-discover-projects-in-search-path)))

(use-package counsel-projectile
  :straight t
  :defer t
  :hook (projectile-mode . counsel-projectile-mode)
  :init
  ;; (counsel-projectile-mode +1)
  (spacemacs/set-leader-keys "p" nil)
  (spacemacs/set-leader-keys "pb" 'counsel-projectile-switch-to-buffer)
  (define-key yq-s-map "B" 'counsel-projectile-switch-to-buffer)
  (spacemacs/set-leader-keys "pf" 'counsel-projectile-find-file)
  (spacemacs/set-leader-keys "pd" 'counsel-projectile-find-dir)
  (spacemacs/set-leader-keys "pl" 'counsel-projectile-switch-project)
  (defun +counsel-projectile-switch-buffer-next-line ()
    (interactive)
    (if (minibufferp) (ivy-next-line)
      (counsel-projectile-switch-to-buffer)))
  (defun +counsel-projectile-switch-buffer-prev-line ()
    (interactive)
    (if (minibufferp) (ivy-previous-line)
      (counsel-projectile-switch-to-buffer))))

  ;; (global-set-key (kbd "C-x C-9 j") '+counsel-projectile-switch-buffer-next-line)
  ;; (global-set-key (kbd "C-x C-9 k") '+counsel-projectile-switch-buffer-prev-line)
  ;; (global-set-key (kbd "C-M-S-s-j") '+counsel-projectile-switch-buffer-next-line)
  ;; (global-set-key (kbd "C-M-S-s-k") '+counsel-projectile-switch-buffer-prev-line)


(spacemacs/set-leader-keys "sd" 'spacemacs/search-dir-rg)
(spacemacs/set-leader-keys "sD" 'spacemacs/search-dir-rg-region-or-symbol)
(spacemacs/set-leader-keys "sf" 'spacemacs/search-auto)
(spacemacs/set-leader-keys "sF" 'spacemacs/search-rg-region-or-symbol)
;; (define-key yq-s-map "sf" 'spacemacs/search-rg-direct)
(define-key yq-s-map "F" 'spacemacs/search-project-rg-region-or-symbol)

(use-feature dired
  :defer t
  :custom
  (dired-listing-switches "-laGh1vt")
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-auto-revert-buffer t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-dwim-target t)
  :init
  (setq insert-directory-program "/usr/local/bin/gls")
  (defvar +binary-file-name-regexp "\\.\\(avi\\|pdf\\|mp[34g]\\|mkv\\|exe\\|3gp\\|rmvb\\|rm\\)$"
    "Is binary file name?")
  (defadvice! +dired-find-file (orig)
    :around #'dired-find-file
    (let* ((file (dired-get-file-for-visit)))
      (cond ((string-match-p +binary-file-name-regexp file)
             ;; confirm before open big file
             (when (yes-or-no-p "Edit binary file?") (call-interactively orig))))))
  :config
  (require 'dired-aux)
  (setq dired-vc-rename-file t
        dired-create-destination-dirs 'always
        dired-isearch-filenames 'dwim)
  (add-to-list 'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip"))
  (add-hook! dired-mode #'hl-line-mode)
  (evil-define-key 'normal dired-mode-map (kbd ";") 'avy-goto-subword-1)
  (evil-define-key 'normal dired-mode-map (kbd "g") nil)
  (evil-define-key 'normal dired-mode-map (kbd "gg") #'evil-goto-first-line)
  (evil-define-key 'normal dired-mode-map (kbd "gr") #'revert-buffer)
  (evil-define-key 'normal dired-mode-map (kbd "gG") #'dired-do-chgrp)
  (evil-define-key 'normal dired-mode-map (kbd "G") #'evil-goto-line)
  (defun ora-ediff-files ()
    (interactive)
    (let ((files (dired-get-marked-files))
          (wnd (current-window-configuration)))
      (if (<= (length files) 2)
          (let ((file1 (car files))
                (file2 (if (cdr files)
                           (cadr files)
                         (read-file-name
                          "file: "
                          (dired-dwim-target-directory)))))
            (if (file-newer-than-file-p file1 file2)
                (ediff-files file2 file1)
              (ediff-files file1 file2))
            (add-hook 'ediff-after-quit-hook-internal
                      (lambda ()
                        (setq ediff-after-quit-hook-internal nil)
                        (set-window-configuration wnd))))
        (error "no more than 2 files should be marked"))))
  (evil-define-key 'normal dired-mode-map "e" 'ora-ediff-files)
  (evil-define-key 'normal dired-mode-map "\\" 'dired-do-async-shell-command)
  (put 'dired-find-alternate-file 'disabled nil)
  (add-hook! dired-mode
            (lambda ()
              (evil-define-key 'normal dired-mode-map (kbd "h")
                (lambda () (interactive) (find-alternate-file "..")))))
  (evil-define-key 'normal dired-mode-map
    "l" #'dired-find-file
    "s" yq-s-map
    "f" #'dired-narrow-fuzzy))


(use-feature wdired
  :defer t
  :config
  (setq wdired-allow-to-redirect-links t
        wdired-allow-to-change-permissions t
        wdired-create-parent-directories t))
(use-package diredfl
  :straight t
  :after dired
  :hook (dired-mode . diredfl-mode))
(use-package dired-narrow
  :straight t
  :commands (dired-narrow-fuzzy))
(use-package fd-dired
  :straight (:host github :repo "yqrashawn/fd-dired")
  :commands (fd-dired)
  :init
  (defadvice! +fd-dired-advice (dir args)
    :before #'fd-dired
    (when (string= (expand-file-name "~/") (expand-file-name (projectile-project-root dir)))
      (setq-local fd-dired-pre-fd-args (concat fd-dired-pre-fd-args " -uuu "))))
  (global-set-key [remap find-dired] #'fd-dired)
  (evil-define-key 'normal dired-mode-map "F" 'fd-dired)
  (define-key yq-s-map "8" 'fd-dired))
(use-feature dired-x
  :hook (dired-mode . dired-omit-mode)
  :commands (dired-jump
             dired-jump-other-window
             dired-omit-mode)
  :custom
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.DS_Store$\\|^__MACOSX$\\|^\\.localized$")
  :config
  (when-let (cmd (cond (IS-MAC "open")
                       (IS-LINUX "xdg-open")
                       (IS-WINDOWS "start")))
    (setq dired-guess-shell-alist-user
          `(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd)))))
(use-package dired+
  :straight (:host github :repo "emacsmirror/dired-plus")
  :defer t
  :custom
  (diredp-hide-details-initially-flag nil)
  :init
  (with-eval-after-load 'dired
    (require 'dired+))
  (setq diredp-bind-problematic-terminal-keys (nin-terminal-p))
  (evil-leader/set-key "fj" 'diredp-dired-recent-dirs)
  (evil-leader/set-key "fJ" 'diredp-dired-recent-dirs-other-window)
  (evil-define-key 'normal dired-mode-map "q" 'yq/kill-this-buffer)
  (evil-define-key 'normal dired-mode-map "h" 'diredp-up-directory-reuse-dir-buffer)
  (evil-define-key 'normal dired-mode-map "j" 'diredp-next-line)
  (evil-define-key 'normal dired-mode-map "k" 'diredp-previous-line)
  (evil-define-key 'normal dired-mode-map "l" 'diredp-find-file-reuse-dir-buffer)
  :config
  (add-hook 'dired-mode-hook '(lambda () (dired-hide-details-mode 0))))
(use-package dired-filter
  ;; :straight (dired-filter :type git :host github :repo "Fuco1/dired-hacks" :files ("dired-filter.el"))
  :straight t
  :hook
  (dired-mode . dired-filter-mode)
  (dired-mode . dired-filter-group-mode)
  :custom
  (dired-filter-stack nil)
  (setq dired-filter-group-saved-groups
        '(("default"
           ("Git" . ((regexp . "^.git")))
           ("Directories" (((not (git-ignored)) (not (regexp . "^.git")) (directory))))
           ("Docker" . ((or (regexp . "^Dockerfile.*") (name . ".dockerignore"))))
           ("Configs" (((not (git-ignored)) (configs))))
           ("Emacs" (extension "el" "eld"))
           ("MarkUp" (extension "md" "org" "txt"))
           ("Archives" (((not (git-ignored)) (extension "zip" "rar" "gz" "bz2" "tar"))))
           ("Images" (((not (git-ignored)) (extension "png" "gif" "jpeg" "jpg"))))
           ("binary" (((not (git-ignored)) (extension "elc" "bin"))))
           ("Ignored" . ((git-ignored))))))
  :config
  (define-key dired-mode-map "N" dired-filter-map)
  (dired-filter-define git-ignored "git ignore without remove"
    (:description "git-ignored" :qualifier-description nil)
    (--any? (f-same? file-name it) qualifier))

  (dired-filter-define configs "config files"
    (:description "config files")
    (or (string-match-p "^\..*\\(lint\\|conf\\|ignore\\).*" file-name)
        (string-match-p "^\..*\\(lock\\|conf\\|toml\\|yaml\\|yml\\|node-version\\)rc$" file-name))))

(use-package dired-git-info
  :straight (:host github :repo "clemera/dired-git-info")
  :commands (dired-git-info-mode)
  :custom
  (dgi-commit-message-format "%h %cs %s")
  (dgi-auto-hide-details-p nil)
  :config
  (define-key dired-mode-map "(" 'dired-git-info-mode))

(use-package dired-quick-sort
  :straight t
  :after dired
  :hook (dired-mode . dired-quick-sort-setup))

(defun yq/dropbox ()
  (interactive)
  (counsel-fzf "" "~/Dropbox/" "Dropbox Files: "))
(defun yq/workspace ()
  (interactive)
  (counsel-fzf "" "~/workspace/"))

(spacemacs/set-leader-keys "f1" 'yq/dropbox)

(defun yq/open-with-call-alfred-osascript (file)
  (shell-command (concat "osascript -e '" (format "-- Search for the file
    tell application \"Alfred 4\"
      search \"%1$s\"
    end tell

    -- Show file actions
    tell application \"System Events\"
      -- Press \"tab\" to show file actions
      key code 48
    end tell'" file))))

(defun yq/open-with-alfred ()
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (yq/open-with-call-alfred-osascript (dired-get-filename nil t))
    (and (file-exists-p buffer-file-name) (yq/open-with-call-alfred-osascript buffer-file-name))))

(spacemacs/set-leader-keys "bb" 'yq/open-with-alfred)

(use-package open-junk-file
  :straight t
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

(use-package fasd
  :straight t
  :init
  (global-fasd-mode 1))

(use-package evil-ex-fasd
  :straight (:host github :repo "yqrashawn/evil-ex-fasd")
  :commands (evil-ex-fasd)
  :init (define-key evil-normal-state-map (kbd "C-f") 'evil-ex-fasd))

(use-package dired-rsync
  :straight t
  :commands (dired-rsync)
  :after dired
  :init
  (bind-key "C-c C-r" 'dired-rsync dired-mode-map))


(yq/update-evil-emacs-state-modes 'ibuffer-mode)

(use-feature ibuffer
  :defer t
  :config
  (require 'ibuf-ext))

(use-feature ibuf-ext
  :defer t
  :config
  (add-to-list 'ibuffer-never-show-predicates "^\\*Ibuffer")
  (add-to-list 'ibuffer-never-show-predicates "^\\*Straight")
  (add-to-list 'ibuffer-never-show-predicates "^\\*:Buffers:")
  (add-to-list 'ibuffer-never-show-predicates "^\\*mu4e"))

(use-package avy
  :straight t
  :defer t
  :custom
  (avy-keys-alist `((avy-goto-char . (?k ?j ?l ?d ?n ?p ?s ?a))
                    (evil-avy-goto-char-timer . (?k ?j ?l ?d ?n ?p ?s ?a))
                    (lispy-ace-symbol . (?j ?k ?l ?a ?s ?d ?f ?h))
                    (lispy-ace-subword . (?j ?k ?l ?a ?s ?d ?f ?h))
                    (lispy-ace-paren . (?j ?k ?l ?a ?s ?d ?f ?h))
                    (lispy-ace-char . (?j ?k ?l ?a ?s ?d ?f ?h))
                    (lispy-ace-replace . (?j ?k ?l ?a ?s ?d ?f ?h))))
  :init
  (setq avy-indent-line-overlay t)
  (setq avy-timeout-seconds 0.3)
  (setq avy-enter-times-out nil)
  (setq avy-background t)
  (setq avy-highlight-first t)
  (define-key yq-s-map "n" 'avy-goto-word-1)
  (define-key yq-s-map "N" 'avy-isearch)
  (define-key yq-s-map "I" 'avy-goto-word-1))

(use-package ace-link
  :straight t
  :commands (ace-link)
  :init
  ;; (define-key evil-normal-state-map "go" #'ace-link)
  ;; (evil-define-key 'normal helpful-mode-map "o" #'ace-link-help)
  (ace-link-setup-default))

(use-package tabbar
  :straight t
  :disabled
  :init
  (setq tabbar-cycle-scope 'tabs
        tabbar-use-images nil
        tabbar-separator (cons 1.2 nil))
  ;; (tabbar-mode 1)
  (defun +tabbar-update-face-depends-on-theme ()
    (set-face-attribute
     'tabbar-default nil
     :background (face-attribute 'default :background)
     :foreground (face-attribute 'font-lock-comment-face :foreground)
     :box '(:line-width -1 :style pressed-button))
    (set-face-attribute
     'tabbar-unselected nil
     :background (face-attribute 'default :background)
     :foreground (face-attribute 'font-lock-doc-face :foreground)
     :box '(:line-width -1 :style pressed-button))
    (set-face-attribute
     'tabbar-selected nil
     :background (face-attribute 'default :background)
     :foreground (face-attribute 'font-lock-keyword-face :foreground)
     :weight 'bold
     :box nil)
    (set-face-attribute
     'tabbar-modified nil
     :background (face-attribute 'default :background)
     :foreground (face-attribute 'error :foreground)
     :box '(:line-width -1 :style pressed-button)
     :weight 'unspecified)
    (set-face-attribute
     'tabbar-selected-modified nil
     :background (face-attribute 'default :background)
     :foreground (face-attribute 'error :foreground)
     :box nil
     :weight 'bold)
    (set-face-attribute
     'tabbar-separator nil
     :inherit 'tabbar-default
     :foreground (face-attribute 'default :background)
     :background (face-attribute 'default :background)
     :box '(:line-width -1 :style pressed-button)))

  ;; hide button
  (customize-set-variable 'tabbar-scroll-right-button '(("") ""))
  (customize-set-variable 'tabbar-scroll-left-button '(("") ""))
  (customize-set-variable 'tabbar-buffer-home-button '(("") ""))
  :config
  (+tabbar-update-face-depends-on-theme)
  (add-hook 'spacemacs-post-theme-change-hook '+tabbar-update-face-depends-on-theme)
  (add-hook 'after-save-hook #'tabbar-display-update)
  (defun tabbar-buffer-list ()
    "Return the list of buffers to show in tabs.
Exclude buffers whose name starts with a space, when they are not
visiting a file.  The current buffer is always included."
    (delq nil
          (mapcar #'(lambda (b)
                      (cond
                       ;; Always include the current buffer.
                       ((eq (current-buffer) b) b)
                       ((string-match-p "\.elc$" (buffer-name b)) nil)
                       ((buffer-file-name b) b)
                       ((string-match-p "^magit.*:\ " (buffer-name b)) nil)
                       ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                       ((char-equal ?*  (aref (buffer-name b) 0)) nil)
                       ((buffer-live-p b) b)))
                  (buffer-list))))
  (defun +tabbar-buffer-groups ()
    "Return the list of group names the current buffer belongs to.
Return a list of one element based on major mode."
    (list
     (cond
      ;; Configs
      ((memq major-mode '(emacs-lisp-mode inferior-emacs-lisp-mode)) "Conf")
      ((and buffer-file-name (or (string-match-p "\.conf$" buffer-file-name)
                                 (string-match-p "\/\.config\/" buffer-file-name)
                                 (string-match-p "prezto" buffer-file-name)
                                 (string-match-p "mbsync" buffer-file-name)
                                 (string-match-p "bitbar" buffer-file-name)
                                 (string-match-p "\.emacs\.d\/" buffer-file-name)
                                 (string-match-p "Dropbox/sync/" buffer-file-name))) "Conf")

      ;; Plan
      ((and buffer-file-name (string-match-p "\/Dropbox\/ORG\/" buffer-file-name)) "Plan")
      ((eq major-mode 'org-agenda-mode) "Plan")

      ;; Clojure
      ;; ((memq major-mode '(clojure-mode clojurescript-mode)) "CLJ")

      ;; Projects
      ((and buffer-file-name (string-match-p "\/workspace\/" buffer-file-name) (projectile-project-name)) (projectile-project-name))

      ;; other prog mode
      ((or (derived-mode-p 'prog-mode) (memq major-mode '(org-mode))) "Working")

      ((eq major-mode 'dired-mode) "Dir")
      ((memq major-mode '(helpful-mode help-mode apropos-mode Info-mode Man-mode)) "Help")
      ((memq major-mode
             '(rmail-mode
               rmail-edit-mode vm-summary-mode vm-mode mail-mode
               mu4e-view-mode mu4e-headers-mode mu4e-main-mode mu4e-org-mode
               mh-letter-mode mh-show-mode mh-folder-mode
               gnus-summary-mode message-mode gnus-group-mode
               gnus-article-mode score-mode gnus-browse-killed-mode))
       "Mail")
      ((or  (char-equal ?\* (aref (buffer-name) 0))
            (char-equal ?\  (aref (buffer-name) 0))) "Common")

      (t "Misc")
      (t
       ;; Return `mode-name' if not blank, `major-mode' otherwise.
       (if (and (stringp mode-name)
                ;; Take care of preserving the match-data because this
                ;; function is called when updating the header line.
                (save-match-data (string-match "[^ ]" mode-name)))
           mode-name
         (symbol-name major-mode))))))

  (defun +tabbar-get-groups ()
    (set tabbar-tabsets-tabset (tabbar-map-tabsets 'tabbar-selected-tab))
    (mapcar #'(lambda (group) (format "%s" (cdr group)))
            (tabbar-tabs tabbar-tabsets-tabset)))

  (defun +tabbar-switch-group (&optional groupname)
    "Switch tab groups using ido."
    (interactive)
    (let* ((tab-buffer-list (mapcar
                             #'(lambda (b)
                                 (with-current-buffer b
                                   (list (current-buffer)
                                         (buffer-name)
                                         (funcall tabbar-buffer-groups-function))))
                             (funcall tabbar-buffer-list-function)))
           (groups (+tabbar-get-groups))
           (group-name (or groupname (ido-completing-read "Groups: " groups))))
      (catch 'done
        (mapc
         #'(lambda (group)
             (when (equal group-name (car (car (cdr (cdr group)))))
               (throw 'done (switch-to-buffer (car (cdr group))))))
         tab-buffer-list))))
  (setq tabbar-buffer-groups-function '+tabbar-buffer-groups)

  (defun counsel-tabbar-groups ()
    (interactive)
    (ivy-read
     "Tabbar Groups:"
     (+tabbar-get-groups)
     :action #'+tabbar-switch-group))

  (defun +tabbar-switch-group-next-line ()
    (interactive)
    (if (minibufferp) (ivy-next-line)
      (counsel-tabbar-groups)))

  (defun +tabbar-switch-group-prevouse-line ()
    (interactive)
    (if (minibufferp) (ivy-previous-line)
      (counsel-tabbar-groups)))

  (defun +tabbar-backward-tab ()
    (interactive)
    (if (and parinfer-mode (evil-insert-state-p))
        (sp-backward-sexp)
      (tabbar-backward-tab)))
  (defun +tabbar-forward-tab-or-ivy-done ()
    (interactive)
    (if (minibufferp)
        (ivy-done)
      (if (and parinfer-mode (evil-insert-state-p))
          (sp-forward-sexp)
        (tabbar-forward-tab))))

  ;; (global-set-key (kbd "C-x C-9 j") #'+tabbar-switch-group-next-line)
  ;; (global-set-key (kbd "C-x C-9 k") #'+tabbar-switch-group-prevouse-line)
  ;; (global-set-key (kbd "C-M-S-s-j") #'+tabbar-switch-group-next-line)
  ;; (global-set-key (kbd "C-M-S-s-k") #'+tabbar-switch-group-prevouse-line)
  (global-set-key (kbd "C-x C-9 p") #'tabbar-backward-group)
  (global-set-key (kbd "C-x C-9 n") #'tabbar-forward-group)
  (global-set-key (kbd "C-x C-9 h") #'tabbar-backward-tab)
  (global-set-key (kbd "C-x C-9 l") #'+tabbar-forward-tab-or-ivy-done)
  (global-set-key (kbd "C-M-S-s-p") #'tabbar-backward-group)
  (global-set-key (kbd "C-M-S-s-n") #'tabbar-forward-group)
  (global-set-key (kbd "C-M-S-s-h") #'+tabbar-backward-tab)
  (global-set-key (kbd "C-M-S-s-l") #'+tabbar-forward-tab-or-ivy-done)
  (defun tabbar-move-current-tab-one-place-left ()
    "Move current tab one place left, unless it's already the leftmost."
    (interactive)
    (let* ((bufset (tabbar-current-tabset t))
           (old-bufs (tabbar-tabs bufset))
           (first-buf (car old-bufs))
           (new-bufs (list)))
      (if (string= (buffer-name) (format "%s" (car first-buf)))
          old-bufs                    ; the current tab is the leftmost
        (setq not-yet-this-buf first-buf)
        (setq old-bufs (cdr old-bufs))
        (while (and
                old-bufs
                (not (string= (buffer-name) (format "%s" (car (car old-bufs))))))
          (push not-yet-this-buf new-bufs)
          (setq not-yet-this-buf (car old-bufs))
          (setq old-bufs (cdr old-bufs)))
        (if old-bufs ; if this is false, then the current tab's buffer name is mysteriously missing
            (progn
              (push (car old-bufs) new-bufs) ; this is the tab that was to be moved
              (push not-yet-this-buf new-bufs)
              (setq new-bufs (reverse new-bufs))
              (setq new-bufs (append new-bufs (cdr old-bufs))))
          (error "Error: current buffer's name was not found in Tabbar's buffer list."))
        (set bufset new-bufs)
        (tabbar-set-template bufset nil)
        (tabbar-display-update))))
  (defun tabbar-move-current-tab-one-place-right ()
    "Move current tab one place right, unless it's already the rightmost."
    (interactive)
    (let* ((bufset (tabbar-current-tabset t))
           (old-bufs (tabbar-tabs bufset))
           (first-buf (car old-bufs))
           (new-bufs (list)))
      (while (and
              old-bufs
              (not (string= (buffer-name) (format "%s" (car (car old-bufs))))))
        (push (car old-bufs) new-bufs)
        (setq old-bufs (cdr old-bufs)))
      (if old-bufs ; if this is false, then the current tab's buffer name is mysteriously missing
          (progn
            (setq the-buffer (car old-bufs))
            (setq old-bufs (cdr old-bufs))
            (if old-bufs ; if this is false, then the current tab is the rightmost
                (push (car old-bufs) new-bufs))
            (push the-buffer new-bufs)) ; this is the tab that was to be moved
        (error "Error: current buffer's name was not found in Tabbar's buffer list."))
      (setq new-bufs (reverse new-bufs))
      (setq new-bufs (append new-bufs (cdr old-bufs)))
      (set bufset new-bufs)
      (tabbar-set-template bufset nil)
      (tabbar-display-update)))
  (global-set-key (kbd "C-x C-9 [") 'tabbar-move-current-tab-one-place-left)
  (global-set-key (kbd "C-x C-9 ]") 'tabbar-move-current-tab-one-place-right)
  (setq +tabbar-tab-keys yq-quick-keys)
  (defun +tabbar-or-ivy-jump (&optional key)
    (interactive)
    (let* ((key (or key (read-char "key: " t))))
      (if (minibufferp)
          (+ivy-select-index key)
        (tabbar-buffer-select-tab
         t (nth (seq-position +tabbar-tab-keys key)
                (tabbar-view tabbar-current-tabset))))))
  (defun tabbar-buffer-tab-label (tab)
    "Return a label for TAB.
That is, a string used to represent it on the tab bar."
    (let* ((tabset (tabbar-tab-tabset tab))
           (index (seq-position
                   (tabbar-view tabset) tab))
           (prefix (if (> index -1)
                       (char-to-string (nth index +tabbar-tab-keys))
                     ""))
           (label (if tabbar--buffer-show-groups
                      (format "[%s]" (tabbar-tab-tabset tab))
                    (format "%s %s" prefix (tabbar-tab-value tab)))))
      ;; Unless the tab bar auto scrolls to keep the selected tab
      ;; visible, shorten the tab label to keep as many tabs as possible
      ;; in the visible area of the tab bar.
      (if tabbar-auto-scroll-flag
          label
        (tabbar-shorten
         label (max 1 (/ (window-width)
                         (length (tabbar-view
                                  (tabbar-current-tabset)))))))))

  (defun +tabbar-kill-other-buffers-in-current-group ()
    (interactive)
    (mapc
     (lambda (buffer-or-nil)
       (when buffer-or-nil
         (kill-buffer buffer-or-nil)))
     (mapcar
      (lambda (tab)
        (when (not (eq (car tab) (current-buffer))) (car tab)))
      (tabbar-tabs tabbar-current-tabset))))
  (spacemacs/set-leader-keys "bd" '+tabbar-kill-other-buffers-in-current-group)
  (define-key tabbar-mode-map (kbd "C-x C-6 1") (lambda () (interactive) (+tabbar-or-ivy-jump ?a)))
  (define-key tabbar-mode-map (kbd "C-x C-6 2") (lambda () (interactive) (+tabbar-or-ivy-jump ?s)))
  (define-key tabbar-mode-map (kbd "C-x C-6 3") (lambda () (interactive) (+tabbar-or-ivy-jump ?d)))
  (define-key tabbar-mode-map (kbd "C-x C-6 4") (lambda () (interactive) (+tabbar-or-ivy-jump ?f)))
  (define-key tabbar-mode-map (kbd "C-x C-6 5") (lambda () (interactive) (+tabbar-or-ivy-jump ?g)))
  (define-key tabbar-mode-map (kbd "C-x C-6 6") (lambda () (interactive) (+tabbar-or-ivy-jump ?h)))
  (define-key tabbar-mode-map (kbd "C-x C-6 7") (lambda () (interactive) (+tabbar-or-ivy-jump ?j)))
  (define-key tabbar-mode-map (kbd "C-x C-6 8") (lambda () (interactive) (+tabbar-or-ivy-jump ?k)))
  (define-key tabbar-mode-map (kbd "C-x C-6 9") (lambda () (interactive) (+tabbar-or-ivy-jump ?l)))
  (define-key tabbar-mode-map (kbd "C-x C-6 0") (lambda () (interactive) (+tabbar-or-ivy-jump 59)))

  ;; disable tabbar track killed buffer
  (defun tabbar-buffer-track-killed ()
    "Hook run just before actually killing a buffer.
In Tabbar mode, try to switch to a buffer in the current tab bar,
after the current buffer has been killed.  Try first the buffer in tab
after the current one, then the buffer in tab before.  On success, put
the sibling buffer in front of the buffer list, so it will be selected
first."))

(use-package loccur
  :straight t
  :commands (loccur-current)
  :init
  (define-key yq-s-map "s" #'loccur-current))

(use-package color-rg
  :straight (:host github :repo "manateelazycat/color-rg")
  :disabled
  :commands (color-rg-search-input
             color-rg-search-symbol
             color-rg-search-project)
  :init
  (spacemacs/set-leader-keys "rg" (defl (if current-prefix-arg
                                            (color-rg-search-project-with-type)
                                          (color-rg-search-project))))
  (define-key yq-s-map "e" #'color-rg-search-project)
  :config
  (evilified-state-evilify color-rg-mode color-rg-mode-map
    (kbd "C-a") 'color-rg-beginning-of-line
    (kbd "<tab>") 'color-rg-jump-next-keyword
    (kbd "<backtab>") 'color-rg-jump-prev-keyword

    (kbd "RET") 'color-rg-open-file
    (kbd "C-m") 'color-rg-open-file

    "r" #'color-rg-replace-all-matches
    "f" #'color-rg-filter-match-results
    "F" #'color-rg-filter-mismatch-results

    "x" #'color-rg-filter-match-files
    "X" #'color-rg-filter-mismatch-files
    "u" #'color-rg-unfilter

    "D" #'color-rg-remove-line-from-results

    "I" #'color-rg-rerun-toggle-ignore
    "t" #'color-rg-rerun-literal
    "c" #'color-rg-rerun-toggle-case
    "s" #'color-rg-rerun-regexp
    "d" #'color-rg-rerun-change-dir
    "z" #'color-rg-rerun-change-files

    "i" (lambda () (interactive) (color-rg-switch-to-edit-mode) (evil-insert-state))
    "q" #'color-rg-quit

    "j" #'color-rg-jump-next-keyword
    "k" #'color-rg-jump-prev-keyword
    "h" #'color-rg-jump-prev-file
    "l" #'color-rg-jump-next-file)

  (evil-define-key 'insert color-rg-mode-edit-map (kbd "<escape>")
    (lambda ()
      (interactive)
      (color-rg-switch-to-view-mode)
      (evil-evilified-state)))

  (define-key color-rg-mode-edit-map (kbd "C-c C-c")
    (lambda ()
      (interactive)
      (color-rg-apply-changed)
      (evil-evilified-state))))

(use-package emacs-chunkwm
  :straight (:host github :repo "yqrashawn/emacs-chunkwm")
  :disabled
  :bind (("C-x 7 w h" . 'emacs-chunkwm-windmove-left)
         ("C-x 7 w l" . 'emacs-chunkwm-windmove-right)
         ("C-x 7 w j" . 'emacs-chunkwm-windmove-down)
         ("C-x 7 w k" . 'emacs-chunkwm-windmove-up)))

(use-package flyspell-correct
  :straight t
  :after flyspell
  :disabled
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

;; (use-package flyspell-correct-ivy
;;   :straight t
;;   :after flyspell-correct)

(use-package flyspell-correct-avy-menu
  :straight t
  :disabled
  :after flyspell-correct)

(use-package helm
  :straight t
  :defer t
  :disabled
  :custom
  (helm-display-buffer-default-height 0.35)
  (helm-default-display-buffer-functions #'display-buffer-in-side-window)
  :bind ("C-`" . helm-top)
  :init
  (define-key yq-s-map "b" #'helm-mini)
  :config
  (define-key helm-map (kbd "C-j") #'helm-next-line)
  (define-key helm-map (kbd "C-k") #'helm-previous-line)
  (define-key helm-map (kbd "C-h") #'helm-next-source)
  (define-key helm-map (kbd "C-l") (kbd "RET"))
  (define-key helm-map (kbd "C-S-j") 'helm-follow-action-forward)
  (define-key helm-map (kbd "C-S-k") 'helm-follow-action-backward)
  (define-key helm-map (kbd "C-S-h") 'describe-key)
  (with-eval-after-load 'helm-files
    (dolist (keymap (list helm-find-files-map helm-read-file-map))
      (define-key keymap (kbd "C-l") 'helm-execute-persistent-action)
      (define-key keymap (kbd "C-h") 'helm-find-files-up-one-level)
      ;; rebind `describe-key' for convenience
      (define-key keymap (kbd "C-S-h") 'describe-key))))

(use-package eyebrowse
  :straight t
  :disabled
  :init
  (eyebrowse-mode t))

(use-package iflipb
  :straight t
  :defer t
  :custom
  (iflipb-always-ignore-buffers
   (lambda (name)
     (print name)
     (or (string-match-p "^magit" name)
         (string-match-p "^\*" name)
         (string-match-p "^ " name))))
  :bind
  (("C-M-S-s-j" . iflipb-next-buffer)
   ("C-x 9 j" . iflipb-next-buffer)
   ("C-M-S-s-k" . iflipb-previous-buffer)
   ("C-x 9 k" . iflipb-previous-buffer)))

(use-package double-saber
  :straight t
  :after (ivy wgrep)
  :defer t
  :init
  (add-hook 'ivy-occur-grep-mode-hook
            (lambda ()
              (double-saber-mode)
              (setq-local double-saber-start-line 5)))
  (defadvice ivy-wgrep-change-to-wgrep-mode (after ivy-wgrep-change-to-wgrep-mode-double-sabber-advice activate)
    "disable `double-saber-mode' when enter wgrep mode"
    (interactive)
    (double-saber-mode -1))

  (defadvice wgrep-finish-edit (after ivy-wgrep-change-to-wgrep-mode-double-sabber-advice activate)
    "enable `double-saber-mode' when leave wgrep mode"
    (interactive)
    (double-saber-mode 1))

  (defadvice wgrep-abort-changes (after ivy-wgrep-change-to-wgrep-mode-double-sabber-advice activate)
    "enable `double-saber-mode' when leave wgrep mode"
    (interactive)
    (double-saber-mode 1))
  :config
  (evil-define-key 'normal double-saber-mode-map "x" #'double-saber-narrow)
  (evil-define-key 'normal double-saber-mode-map "d" #'double-saber-delete)
  (evil-define-key 'normal double-saber-mode-map "S" #'double-saber-sort-lines)
  (evil-define-key 'normal double-saber-mode-map "u" #'double-saber-undo)
  (evil-define-key 'normal double-saber-mode-map (kbd "C-r") #'double-saber-redo))

(use-package rg
  :straight t
  :commands (rg rg-literal rg-dwim rg-project))

(use-package bm
  :straight t
  :custom
  ;; restore on load (even before you require bm)
  (bm-restore-repository-on-load t)
  :config
  ;; Allow cross-buffer 'next'
  (setq bm-cycle-all-buffers t)
  ;; where to store persistant files
  (setq bm-repository-file "~/.emacs.d/bm-repository")
  ;; save bookmarks
  (setq-default bm-buffer-persistence t)
  ;; Loading the repository from file when on start up.
  (add-hook 'after-init-hook 'bm-repository-load)
  ;; Saving bookmarks
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when Emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))
  ;; The `after-save-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state.
  (add-hook 'after-save-hook #'bm-buffer-save)
  ;; Restoring bookmarks
  (add-hook 'find-file-hooks #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)

  ;; The `after-revert-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state. This hook might cause trouble when using packages
  ;; that automatically reverts the buffer (like vc after a check-in).
  ;; This can easily be avoided if the package provides a hook that is
  ;; called before the buffer is reverted (like `vc-before-checkin-hook').
  ;; Then new bookmarks can be saved before the buffer is reverted.
  ;; Make sure bookmarks is saved before check-in (and revert-buffer)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
  :bind (("C-c b j" . bm-next)
         ("C-c b k" . bm-previous)
         ("C-c b b" . bm-toggle)))

(use-package golden-ratio
  :straight t
  :hook (after-init . golden-ratio-mode))

(use-package bicycle
  :straight t
  :after outline
  :bind (:map outline-minor-mode-map
              ([tab] . bicycle-cycle)
              ([S-tab] . bicycle-cycle)
              ("C-i" . bicycle-cycle)
              ("<backtab>" . bicycle-cycle-global)))

(use-package find-file-in-project
  :straight t
  :custom
  (ffip-use-rust-fd t)
  :bind ((:map yq-s-map ("m" . ffip)))
  :init
  (spacemacs/set-leader-keys "krp" #'ffip-find-relative-path))

(use-package prot-outline
  :disabled t
  :straight (:host gitlab :repo "protesilaos/dotfiles" :branch "master"
                   :files ("emacs/.emacs.d/prot-lisp/prot-common.el"
                           "emacs/.emacs.d/prot-lisp/prot-outline.el"))
  :hook (prog-mode . prot-outline-minor-mode-safe)
  :config
  (with-eval-after-load 'outline-minor-faces
    (add-hook 'prot-outline-minor-mode-enter-hook #'outline-minor-faces-add-font-lock-keywords)))

(use-package 'selectrum
  :straight t
  :disabled
  :custom
  (selectrum-refine-candidates-function orderless-filter)
  (selectrum-highlight-candidates-function orderless-highlight-matches)
  :config
  (let ((map selectrum-minibuffer-map))
    (define-key map [remap keyboard-quit] #'abort-recursive-edit)
    ;; This is bound in `minibuffer-local-map' by loading `delsel', so
    ;; we have to account for it too.
    (define-key map [remap minibuffer-keyboard-quit]
      #'abort-recursive-edit)
    ;; Override both the arrow keys and C-n/C-p.
    ;; (define-key map [remap previous-line] #'selectrum-previous-candidate)
    (define-key map "\C-k" #'selectrum-previous-candidate)
    ;; (define-key map [remap next-line] #'selectrum-next-candidate)
    (define-key map "\C-j" #'selectrum-next-candidate)
    (define-key map [remap previous-line-or-history-element] #'selectrum-previous-candidate)
    (define-key map "\C-p" #'selectrum-previous-candidate)
    (define-key map [remap next-line-or-history-element] #'selectrum-next-candidate)
    (define-key map "\C-n" #'selectrum-next-candidate)
    (define-key map [remap exit-minibuffer] #'selectrum-select-current-candidate)
    (define-key map "\C-l" #'selectrum-select-current-candidate)
    (define-key map [remap scroll-down-command] #'selectrum-previous-page)
    (define-key map [remap scroll-up-command] #'selectrum-next-page)
    ;; Use `minibuffer-beginning-of-buffer' for Emacs >=27 and
    ;; `beginning-of-buffer' for Emacs <=26.
    (define-key map [remap minibuffer-beginning-of-buffer] #'selectrum-goto-beginning)
    (define-key map [remap beginning-of-buffer] #'selectrum-goto-beginning)
    (define-key map [remap end-of-buffer] #'selectrum-goto-end)
    (define-key map [remap kill-ring-save] #'selectrum-kill-ring-save)
    (define-key map [remap previous-matching-history-element] #'selectrum-select-from-history)
    (define-key map [remap backward-kill-sexp] #'selectrum-backward-kill-sexp)
    (define-key map (kbd "C-M-DEL") #'selectrum-backward-kill-sexp)
    (define-key map (kbd "C-w") #'selectrum-backward-kill-sexp)
    (define-key map (kbd "C-M-<backspace>") #'selectrum-backward-kill-sexp)
    (define-key map (kbd "C-M-j") #'selectrum-submit-exact-input)
    (define-key map (kbd "TAB") #'selectrum-insert-current-candidate)
    (define-key map (kbd "M-q") 'selectrum-cycle-display-style)
    (define-key map (kbd "M-i") 'selectrum-quick-insert)
    (define-key map (kbd "M-m") 'selectrum-quick-select)
    ;; Return the map.
    map))

(use-package better-jumper
  :straight t
  :hook (after-init . better-jumper-mode)
  :custom
  (better-jumper-context 'window)
  (better-jumper-new-window-behavior 'copy)
  (better-jumper-add-jump-behavior 'replace)
  (better-jumper-max-length 1000)
  (better-jumper-use-evil-jump-advice t)
  :init
  (global-set-key [remap evil-jump-forward] #'better-jumper-jump-forward)
  (global-set-key [remap evil-jump-backward] #'better-jumper-jump-backward)
  (global-set-key [remap xref-pop-marker-stack] #'better-jumper-jump-backward)
  :config
  ;; from doom-emacs
  (defun doom-set-jump-a (orig-fn &rest args)
    "Set a jump point and ensure ORIG-FN doesn't set any new jump points."
    (better-jumper-set-jump (if (markerp (car args)) (car args)))
    (let ((evil--jumps-jumping t)
          (better-jumper--jumping t))
      (apply orig-fn args)))

  (defun doom-set-jump-maybe-a (orig-fn &rest args)
    "Set a jump point if ORIG-FN returns non-nil."
    (let ((origin (point-marker))
          (result
           (let* ((evil--jumps-jumping t)
                  (better-jumper--jumping t))
             (apply orig-fn args))))
      (unless result
        (with-current-buffer (marker-buffer origin)
          (better-jumper-set-jump
           (if (markerp (car args))
               (car args)
             origin))))
      result))
  ;; Creates a jump point before killing a buffer. This allows you to undo
  ;; killing a buffer easily (only works with file buffers though; it's not
  ;; possible to resurrect special buffers).
  (advice-add #'kill-current-buffer :around #'doom-set-jump-a)

  ;; Create a jump point before jumping with imenu.
  (advice-add #'imenu :around #'doom-set-jump-a)

  (defun doom-set-jump-h ()
    "Run `better-jumper-set-jump' but return nil, for short-circuiting hooks."
    (better-jumper-set-jump)
    nil)

  (defun +ivy/jump-list ()
    "Go to an entry in evil's (or better-jumper's) jumplist."
    (interactive)
    ;; REVIEW Refactor me
    (let (buffers)
      (unwind-protect
          (ivy-read "jumplist: "
                    (nreverse
                     (delete-dups
                      (delq
                       nil
                       (mapcar (lambda (mark)
                                 (when mark
                                   (cl-destructuring-bind (path pt _id) mark
                                     (let ((buf (get-file-buffer path)))
                                       (unless buf
                                         (push (setq buf (find-file-noselect path t))
                                               buffers))
                                       (with-current-buffer buf
                                         (goto-char pt)
                                         (font-lock-fontify-region (line-beginning-position) (line-end-position))
                                         (cons (format "%s:%d: %s"
                                                       (buffer-name)
                                                       (line-number-at-pos)
                                                       (string-trim-right (or (thing-at-point 'line) "")))
                                               (point-marker)))))))
                               (cddr (better-jumper-jump-list-struct-ring
                                      (better-jumper-get-jumps (better-jumper--get-current-context))))))))
                    :sort nil
                    :require-match t
                    :action (lambda (cand)
                              (let ((mark (cdr cand)))
                                (delq! (marker-buffer mark) buffers)
                                (mapc #'kill-buffer buffers)
                                (setq buffers nil)
                                (with-current-buffer (switch-to-buffer (marker-buffer mark))
                                  (goto-char (marker-position mark)))))
                    :caller '+ivy/jump-list)
        (mapc #'kill-buffer buffers))))
  (define-key yq-s-map "J" '+ivy/jump-list))
