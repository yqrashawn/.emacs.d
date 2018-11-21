(use-package swiper
  :straight (:host github :repo "abo-abo/swiper" :branch "master"
                   :files ("swiper.el")
                   :upstream (:host github :repo "abo-abo/swiper"))
  :config
  ;; (global-set-key (kbd "C-SPC") 'counsel-grep-or-swiper)
  ;; (global-set-key (kbd "^@") 'counsel-grep-or-swiper)
  ;; (define-key evil-normal-state-map (kbd "sn") 'spacemacs/swiper-all-region-or-symbol)
  (global-set-key (kbd "C-SPC") 'swiper)
  (global-set-key (kbd "^@") 'swiper)
  (global-set-key (kbd "C-S-SPC") 'spacemacs/swiper-region-or-symbol))

(use-package counsel
  :straight (:host github :repo "abo-abo/swiper" :branch "master"
                   :files ("counsel.el")
                   :upstream (:host github :repo "abo-abo/swiper"))
  :diminish counsel-mode
  :config
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
  (define-key ivy-minibuffer-map (kbd "C-w") 'backward-kill-word)
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (spacemacs/set-leader-keys "<SPC>" 'counsel-M-x)
  (global-set-key (kbd "s-j") 'counsel-M-x)
  (global-set-key (kbd "s-f") 'counsel-M-x)
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
  (spacemacs/set-leader-keys "hdl" 'view-lossage)
  (spacemacs/set-leader-keys "fJ" 'spacemacs/open-junk-file)
  (define-key evil-normal-state-map "sf" 'counsel-rg)
  (define-key evil-normal-state-map "sl" 'spacemacs/counsel-jump-in-buffer)
  (define-key evil-normal-state-map "sj" 'counsel-recentf)
  (define-key evil-normal-state-map (kbd "s SPC") 'counsel-M-x)
  (define-key evil-normal-state-map (kbd "M-y" ) 'counsel-yank-pop)
  (defun counsel-recent-dir ()
    "Goto recent directories."
    (interactive)
    (unless recentf-mode (recentf-mode 1))
    (let* ((cands (delete-dups
                   (append my-dired-directory-history
                           (mapcar 'file-name-directory recentf-list)
                           ;; fasd history
                           (if (executable-find "fasd")
                               (split-string (shell-command-to-string "fasd -ld") "\n" t))))))
      (ivy-read "directories:" cands :action 'dired)))
  (define-key evil-normal-state-map "sy" 'counsel-recent-dir))

(use-package counsel-tramp
  :straight t
  :commands (counsel-tramp)
  :init
  ;; if zsh still failed
  ;; https://www.emacswiki.org/emacs/TrampMode#toc7
  ;; (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
  (spacemacs/set-leader-keys "fT" 'counsel-tramp))

(use-package imenu-anywhere
  :straight t
  :commands (imenu-anywhere)
  :init (define-key evil-normal-state-map "sL" 'imenu-anywhere))

(use-package helpful
  :straight t
  :after counsel
  :commands (helpful-variable helpful-key helpful-function)
  :init
  (spacemacs/set-leader-keys "hdV" 'helpful-variable)
  (spacemacs/set-leader-keys "hdv" 'describe-variable)
  (spacemacs/set-leader-keys "hdk" 'helpful-key)
  (spacemacs/set-leader-keys "hdF" 'helpful-function)
  (spacemacs/set-leader-keys "hdf" 'describe-function)
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

(use-package ivy
  :straight (:host github :repo "abo-abo/swiper" :branch "master"
                   :files (:defaults
                           (:exclude "swiper.el" "counsel.el" "ivy-hydra.el")
                           "doc/ivy-help.org")
                   :upstream (:host github :repo "abo-abo/swiper"))
  :diminish ivy-mode
  ;; :init
  ;; (add-to-list 'ivy-re-builders-alist '(t . spacemacs/ivy--regex-plus))
  :config
  (defun +ivy-switch-buffer-next-line ()
    (interactive)
    (if (minibufferp) (ivy-next-line)
      (ivy-switch-buffer)))
  (defun +ivy-switch-buffer-prev-line ()
    (interactive)
    (if (minibufferp) (ivy-previous-line)
      (ivy-switch-buffer)))

  (global-set-key (kbd "C-x C-9 j") '+ivy-switch-buffer-next-line)
  (global-set-key (kbd "C-x C-9 k") '+ivy-switch-buffer-prev-line)
  (global-set-key (kbd "C-M-S-s-j") '+ivy-switch-buffer-next-line)
  (global-set-key (kbd "C-M-S-s-k") '+ivy-switch-buffer-prev-line)
  ;; (global-set-key (kbd "C-x C-8 l") 'ivy-alt-done)
  ;; (global-set-key (kbd "C-x C-8 j") '+ivy-switch-buffer-next-line)
  ;; (global-set-key (kbd "C-x C-8 k") '+ivy-switch-buffer-prev-line)
  ;; (global-set-key (kbd "C-x C-8 a" ) 'ivy-beginning-of-buffer)
  ;; (global-set-key (kbd "C-x C-8 e" ) 'ivy-end-of-buffer)
  ;; (global-set-key (kbd "C-x C-8 u" ) 'ivy-scroll-down-command)
  ;; (global-set-key (kbd "C-x C-8 d" ) 'ivy-scroll-up-command)
  (ivy-mode 1)
  (setq ivy-height 16)
  (setq ivy-use-virtual-buffers t)
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
  (evil-define-key 'normal ivy-occur-mode-map "w" 'ivy-wgrep-change-to-wgrep-mode)
  (ivy-set-occur 'spacemacs/counsel-search
                 'spacemacs//counsel-occur)
  (evil-make-overriding-map ivy-occur-mode-map 'normal)
  (define-key evil-normal-state-map "sB" 'ivy-switch-buffer))

(use-package ivy-hydra
  :straight t
  :after ivy
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
     :exit t)))

(use-package wgrep
  :straight t
  :defer t
  :init
  (setq wgrep-auto-save-buffer t))

(use-package smex
  :straight t
  :init
  (setq-default smex-history-length 100)
  (setq-default smex-save-file (concat yq-emacs-cache-dir ".smex-items")))

(use-package projectile
  :straight t
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (define-key evil-normal-state-map "sJ" 'projectile-recentf))

(use-package counsel-projectile
  :straight t
  :init
  (counsel-projectile-mode +1)
  (spacemacs/set-leader-keys "p" nil)
  (spacemacs/set-leader-keys "pb" 'counsel-projectile-switch-to-buffer)
  (define-key evil-normal-state-map "sb" 'counsel-projectile-switch-to-buffer)
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
      (counsel-projectile-switch-to-buffer)))

  ;; (global-set-key (kbd "C-x C-9 j") '+counsel-projectile-switch-buffer-next-line)
  ;; (global-set-key (kbd "C-x C-9 k") '+counsel-projectile-switch-buffer-prev-line)
  ;; (global-set-key (kbd "C-M-S-s-j") '+counsel-projectile-switch-buffer-next-line)
  ;; (global-set-key (kbd "C-M-S-s-k") '+counsel-projectile-switch-buffer-prev-line)
  (defun yq/.emacs.d ()
    (interactive)
    (counsel-fd "" "~/.emacs.d/" nil "-t f --no-ignore-vcs -E 'straight/build/*'"))
  (defun yq/.emacs.d.el ()
    (interactive)
    (counsel-fd "" "~/.emacs.d/" nil "-t f -e el --no-ignore-vcs -E 'straight/build/*'"))
  (spacemacs/set-leader-keys "fef" 'yq/.emacs.d.el)
  (spacemacs/set-leader-keys "feF" 'yq/.emacs.d)
  (spacemacs/set-leader-keys "fel" 'counsel-find-library))

(yq/get-modules "counsel-funcs.el")
(ivy-set-actions
 'counsel-recentf
 spacemacs--ivy-file-actions)

(spacemacs/set-leader-keys "s" nil)
(spacemacs/set-leader-keys "sd" 'spacemacs/search-dir-rg)
(spacemacs/set-leader-keys "sD" 'spacemacs/search-dir-rg-region-or-symbol)
(spacemacs/set-leader-keys "sf" 'spacemacs/search-auto)
(spacemacs/set-leader-keys "sF" 'spacemacs/search-rg-region-or-symbol)
;; (define-key evil-normal-state-map "sf" 'spacemacs/search-rg-direct)
(define-key evil-normal-state-map "sF" 'spacemacs/search-project-rg-region-or-symbol)

(use-package dired
  :init
  (setq insert-directory-program "/usr/local/bin/gls")
  (setq dired-listing-switches "-laGh1v")
  (setq dired-recursive-deletes 'always)
  ;; https://www.emacswiki.org/emacs/EmacsSession which is easier to setup than "desktop.el"
  ;; See `session-globals-regexp' in "session.el".
  ;; If the variable is named like "*-history", it will be automaticlaly saved.
  (defvar my-dired-directory-history nil "Recent directories accessed by dired.")
  (defvar binary-file-name-regexp "\\.\\(avi\\|pdf\\|mp[34g]\\|mkv\\|exe\\|3gp\\|rmvb\\|rm\\)$"
    "Is binary file name?")
  ;; avoid accidently edit huge media file in dired
  (defadvice dired-find-file (around dired-find-file-hack activate)
    (let* ((file (dired-get-file-for-visit)))
      (cond
       ((string-match-p binary-file-name-regexp file)
        ;; confirm before open big file
        (if (yes-or-no-p "Edit binary file?") ad-do-it))
       (t
        (when (file-directory-p file)
          (add-to-list 'my-dired-directory-history file))
        ad-do-it))))
  (defun my-dired-frame (directory)
    "Open up a dired frame which closes on exit."
    (interactive)
    (switch-to-buffer (dired directory))
    (local-set-key
     (kbd "C-x C-c")
     (lambda ()
       (interactive)
       (kill-this-buffer)
       (save-buffers-kill-terminal 't))))
  :config
  (evil-define-key 'normal dired-mode-map (kbd ";") 'avy-goto-subword-1)
  ;; search file name only when focus is over file
  (setq dired-isearch-filenames 'dwim)
  ;; when there is two dired buffer, Emacs will select another buffer
  ;; as target buffer (target for copying files, for example).
  ;; It's similar to windows commander.
  (setq dired-dwim-target t)
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
  (add-hook 'dired-mode-hook
            (lambda ()
              (evil-define-key 'normal dired-mode-map (kbd "h")
                (lambda () (interactive) (find-alternate-file "..")))))
  (defadvice dired-advertised-find-file (around dired-subst-directory activate)
    "Replace current buffer if file is a directory."
    (interactive)
    (let ((orig (current-buffer))
          (filename (dired-get-filename)))
      ad-do-it
      (when (and (file-directory-p filename)
                 (not (eq (current-buffer) orig)))
        (kill-buffer orig))))
  (evil-define-key 'normal dired-mode-map
    "l" 'dired-find-file
    "s" 'nil
    "sk" 'yq/kill-this-buffer
    "sj" #'counsel-recentf
    "sJ" #'projectile-recentf
    "sb" #'projectile-switch-to-buffer
    "sm" #'find-file-in-project
    "sB" #'ivy-switch-buffer
    (kbd "s SPC") 'counsel-M-x
    "sf" #'counsel-rg
    "ss" #'dired-sort-toggle-or-edit
    "sc" 'yq/delete-window
    "f" #'dired-narrow-fuzzy))
(use-package dired-narrow
  :straight t
  :after dired
  :commands (dired-narrow-fuzzy))
(use-package fd-dired
  :straight (:host github :repo "yqrashawn/fd-dired")
  :after dired
  :init
  (evil-define-key 'normal dired-mode-map "F" 'fd-dired)
  (define-key evil-normal-state-map "s8" 'fd-dired))
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
(use-package dired-filter
  :straight t
  :hook (dired-mode . dired-filter-mode))
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

  (or (counsel-more-chars)
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

(defun counsel-fd (&optional initial-input initial-directory fd-prompt fd-args)
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
  (ivy-set-prompt 'counsel-fd #'counsel-prompt-function-default)
  (setq counsel-fd-current-dir (or initial-directory default-directory))
  (ivy-read (or fd-prompt (car (split-string counsel-fd-base-command)))
            (lambda (string)
              (counsel-fd-function string (concat counsel-fd-base-command " " (or fd-args " "))))
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

(defun yq/find-org|gtd () (interactive) (find-file "~/Dropbox/ORG/gtd.org"))
(defun yq/find-org|project () (interactive) (find-file "~/Dropbox/ORG/project.org"))
;; (spacemacs/set-leader-keys "3" 'yq/find-org|gtd)
;; (spacemacs/set-leader-keys "4" 'yq/find-org|project)
;; (spacemacs/set-leader-keys "sm" 'counsel-fd)

(defun yq/org ()
  (interactive)
  (counsel-fd "" "~/Dropbox/" nil "-t f -e org"))

(defun yq/books ()
  (interactive)
  (counsel-fd "" "~/Dropbox/Books/" nil "-t f"))

(defun yq/dropbox ()
  (interactive)
  (counsel-fd "" "~/Dropbox/"))

(spacemacs/set-leader-keys "fo" 'yq/org)
(spacemacs/set-leader-keys "fb" 'yq/books)
(spacemacs/set-leader-keys "f1" 'yq/dropbox)

(defun yq/open-with-call-alfred-osascript (file)
  (shell-command (concat "osascript -e '" (format "-- Search for the file
    tell application \"Alfred 3\"
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

(define-key evil-normal-state-map (kbd "s.") 'yq/open-with-alfred)
(spacemacs/set-leader-keys "bb" 'yq/open-with-alfred)

(use-package find-file-in-project
  :straight t
  :commands (find-file-in-project)
  :init
  (setq ffip-use-rust-fd t)
  (spacemacs/set-leader-keys "pf" 'find-file-in-project)
  (spacemacs/set-leader-keys "pF" 'find-relative-path)
  (spacemacs/set-leader-keys "sm" 'find-file-in-project)
  (spacemacs/set-leader-keys "sM" 'find-relative-path)
  (evil-define-key 'normal dired-mode-map "sm" 'find-file-in-project)
  (define-key evil-normal-state-map "sm" 'find-file-in-project)
  (define-key evil-normal-state-map "sM" 'find-relative-path))

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

(use-package fasd
  :straight t
  :init
  (global-fasd-mode 1))

(use-package evil-ex-fasd
  :straight (:host github :repo "yqrashawn/evil-ex-fasd")
  :init (define-key evil-normal-state-map (kbd "C-f") 'evil-ex-fasd))

(use-package dired-rsync
  :straight t
  :commands (dired-rsync)
  :init
  (bind-key "C-c C-r" 'dired-rsync dired-mode-map))

(yq/update-evil-emacs-state-modes 'ibuffer-mode)
;; (push 'ibuffer-mode evil-insert-state-modes)
;; (define-key ibuffer-mode-map "j" 'ibuffer-forward-line)
;; (define-key ibuffer-mode-map "k" 'ibuffer-backward-line)
(add-to-list 'ibuffer-never-show-predicates "^\\*Ibuffer")
(add-to-list 'ibuffer-never-show-predicates "^\\*Straight")
;; (add-to-list 'ibuffer-never-show-predicates "^\\*scratch")
;; (add-to-list 'ibuffer-never-show-predicates "^\\*Messages")
;; (add-to-list 'ibuffer-never-show-predicates "^\\*Warnings")
(add-to-list 'ibuffer-never-show-predicates "^\\*:Buffers:")
(add-to-list 'ibuffer-never-show-predicates "^\\*mu4e")
;; (add-to-list 'ibuffer-never-show-predicates "^\\*Help")

;; (use-package deadgrep
;;   :straight t
;;   :commands (deadgrep)
;;   :config
;;   (evil-define-key 'normal deadgrep-mode-map (kbd "RET") 'deadgrep-visit-result)
;;   (evil-define-key 'normal deadgrep-mode-map (kbd "TAB") 'deadgrep-toggle-file-results))

(use-package avy
  :straight t
  :init
  (define-key evil-normal-state-map "su" 'avy-goto-word-1-above)
  (define-key evil-normal-state-map "sn" 'avy-goto-word-1-below)
  (define-key evil-normal-state-map "sI" 'avy-goto-char-2))

(use-package rg
  :straight t
  :commands (rg rg-dwim rg-literal rg-project)
  :init
  (rg-enable-default-bindings)
  (evil-leader/set-key "rg" #'rg)
  (evil-leader/set-key "rG" #'rg-literal)
  (evilified-state-evilify rg-mode rg-mode-map
    "C-n" 'next-error-no-select
    "C-p" 'previous-error-no-select
    "F" 'rg-rerun-change-files
    "L" 'rg-list-searches
    "I" 'rg-rerun-toggle-ignore
    "R" 'rg-rerun-change-regexp))
  ;; (evilified-state-evilify-map  rg-mode-map :mode rg-mode))

(use-package ace-link
  :straight t
  :commands (ace-link)
  :init
  (define-key evil-normal-state-map "go" #'ace-link)
  (evil-define-key 'normal helpful-mode-map "o" #'ace-link-help)
  (ace-link-setup-default))

;; (use-package frames-only-mode
;;   :straight t
;;   :commands (frames-only-mode)
;;   :init (frames-only-mode 1))

(use-package awesome-tab
  :straight (:host github :repo "manateelazycat/awesome-tab")
  :after projectile
  :init
  (defface awesome-tab-default
    '((t :inherit default :height 1 :box (:line-width 1 :color "dark cyan" :style released-button)))
    "Default face used in the tab bar." :group 'awesome-tab)
  (defface awesome-tab-unselected
    '((t (:inherit awesome-tab-default)))
    "Face used for unselected tabs." :group 'awesome-tab)
  (defface awesome-tab-selected
    '((t (:inherit font-lock-keyword-face :weight ultra-bold :box (:line-width 1 :color "dark cyan" :style released-button))))
    "Face used for the selected tab." :group 'awesome-tab)
  (defface awesome-tab-button
    '((t :inherit awesome-tab-default :foreground "dark red"))
    "Face used for tab bar buttons." :group 'awesome-tab)
  (setq awesome-tab-cycle-scope 'tabs)
  (add-hook 'spacemacs-post-theme-change-hook (lambda () (awesome-tab-mode 1)))
  :config
  (defun awesome-tab-switch-group (&optional groupname)
    "Switch tab groups using ido."
    (interactive)
    (let* ((tab-buffer-list (mapcar
                             #'(lambda (b)
                                 (with-current-buffer b
                                   (list (current-buffer)
                                         (buffer-name)
                                         (funcall awesome-tab-buffer-groups-function))))
                             (funcall awesome-tab-buffer-list-function)))
           (groups (awesome-tab-get-groups))
           (group-name (or groupname (ivy-read "Groups: " groups))))
      (catch 'done
        (mapc
         #'(lambda (group)
             (when (equal group-name (car (car (cdr (cdr group)))))
               (throw 'done (switch-to-buffer (car (cdr group))))))
         tab-buffer-list))))
  (defun +awesome-tab-switch-group-next-line ()
    (interactive)
    (if (minibufferp) ( ivy-next-line)
      (awesome-tab-switch-group)))
  (defun +awesome-tab-switch-group-prevouse-line ()
    (interactive)
    (if (minibufferp) (ivy-previous-line)
      (awesome-tab-switch-group)))
  (defun +awesome-tab-forward-tab-or-ivy-done ()
    (interactive)
    (if (minibufferp) (ivy-done)
      (awesome-tab-forward-tab)))
  ;; disabled, using ivy-switch-buffer
  ;; (global-set-key (kbd "C-x C-9 i") #'awesome-tab-select-beg-tab)
  ;; (global-set-key (kbd "C-x C-9 o") #'awesome-tab-select-end-tab)
  ;; (global-set-key (kbd "C-x C-9 j") '+awesome-tab-switch-group-next-line)
  ;; (global-set-key (kbd "C-x C-9 k") '+awesome-tab-switch-group-prevouse-line)
  ;; trackpad
  (global-set-key (kbd "s-{") #'awesome-tab-backward-tab)
  (global-set-key (kbd "s-}") '+awesome-tab-forward-tab-or-ivy-done)

  ;; terminal without repeat
  (global-set-key (kbd "C-x C-9 h") #'awesome-tab-backward-tab)
  (global-set-key (kbd "C-x C-9 l") '+awesome-tab-forward-tab-or-ivy-done)
  (global-set-key (kbd "C-x C-9 n") '+awesome-tab-switch-group-next-line)
  (global-set-key (kbd "C-x C-9 p") '+awesome-tab-switch-group-prevouse-line)
  (global-set-key (kbd "C-x C-9 [") #'awesome-tab-move-current-tab-to-left)
  (global-set-key (kbd "C-x C-9 ]") #'awesome-tab-move-current-tab-to-right)

  ;; gui with repeat
  (global-set-key (kbd "C-M-S-s-h") #'awesome-tab-backward-tab)
  (global-set-key (kbd "C-M-S-s-l") '+awesome-tab-forward-tab-or-ivy-done)
  (global-set-key (kbd "C-M-S-s-n") '+awesome-tab-switch-group-next-line)
  (global-set-key (kbd "C-M-S-s-p") '+awesome-tab-switch-group-prevouse-line))


(use-package loccur
  :straight t
  :init
  (loccur-mode 1)
  (define-key evil-normal-state-map "ss" #'loccur-current))