(use-package swiper
  :straight (:host github :repo "abo-abo/swiper" :branch "master"
                   :files ("swiper.el")
                   :upstream (:host github :repo "abo-abo/swiper"))
  :config
  ;; (global-set-key (kbd "C-SPC") 'counsel-grep-or-swiper)
  ;; (global-set-key (kbd "^@") 'counsel-grep-or-swiper)
  (define-key evil-normal-state-map (kbd "sn") 'spacemacs/swiper-all-region-or-symbol)
  (global-set-key (kbd "C-SPC") 'swiper)
  (global-set-key (kbd "^@") 'swiper)
  (global-set-key (kbd "C-S-SPC") 'spacemacs/swiper-region-or-symbol))

(use-package counsel
  :straight (:host github :repo "abo-abo/swiper" :branch "master"
                   :files ("counsel.el")
                   :upstream (:host github :repo "abo-abo/swiper"))
  :diminish counsel-mode
  :config
  (counsel-mode 1)
  (setq counsel-find-file-occur-cmd "ls | grep -i -E '%s' | gxargs -d '\n' ls")
  (define-key ivy-minibuffer-map (kbd "C-c C-e") 'spacemacs//counsel-edit)
  (define-key ivy-minibuffer-map (kbd "C-n") 'ivy-next-history-element)
  (define-key ivy-minibuffer-map (kbd "C-p") 'ivy-previous-history-element)
  (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
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
  (define-key evil-normal-state-map "so" 'counsel-recent-dir))
(use-package counsel-tramp
  :straight t
  :commands (counsel-tramp)
  :init
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
  (spacemacs/set-leader-keys "hdv" 'helpful-variable)
  (spacemacs/set-leader-keys "hdk" 'helpful-key)
  (spacemacs/set-leader-keys "hdf" 'helpful-function)
  (evil-define-key 'normal helpful-mode-map "Q"
    (lambda ()
      (interactive)
      (quit-window)
      (yq/delete-window)))
  (defun yq/kill-buffer-and-window ()
    (interactive)
    (kill-current-buffer)
    (yq/delete-window))
  (evil-define-key 'normal helpful-mode-map "q" 'yq/kill-buffer-and-window))

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
  (evil-make-overriding-map ivy-occur-mode-map 'normal))
;; (define-key evil-normal-state-map "sb" 'ivy-switch-buffer)

(use-package ivy-hydra
  :straight t
  :after ivy
  :config
  (defun yq/ivy-call-kill-buffer-action ()
    "Call the current action without exiting completion."
    (interactive)
    ;; (unless
    ;;     (or
    ;;      ;; this is needed for testing in ivy-with which seems to call ivy-call
    ;;      ;; again, and this-command is nil in that case.
    ;;      (null this-command)
    ;;      (memq this-command '(ivy-done
    ;;                           ivy-alt-done
    ;;                           ivy-dispatching-done)))
    ;;   (setq ivy-current-prefix-arg current-prefix-arg))
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
  :defer t)

(use-package smex
  :straight t
  :init
  (setq-default smex-history-length 32)
  (setq-default smex-save-file (concat yq-emacs-cache-dir ".smex-items")))

(use-package projectile
  :straight t
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :straight t
  :commands (counsel-projectile-mode)
  :init
  (counsel-projectile-mode)
  (spacemacs/set-leader-keys "p" nil)
  (spacemacs/set-leader-keys "pb" 'counsel-projectile)
  (spacemacs/set-leader-keys "pf" 'counsel-projectile-find-file)
  (spacemacs/set-leader-keys "pd" 'counsel-projectile-find-dir)
  (spacemacs/set-leader-keys "pl" 'counsel-projectile-switch-project)
  (defun yq/find-emacsd-modules ()
    "find file in .emacs.d"
    (interactive) (counsel-projectile-switch-project-by-name "~/.emacs.d"))
  (spacemacs/set-leader-keys "fef" 'yq/find-emacsd-modules)
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
    "sj" 'counsel-recentf
    (kbd "s SPC") 'counsel-M-x
    "sf" 'counsel-rg
    "ss" 'dired-sort-toggle-or-edit
    "sc" 'yq/delete-window
    "f" 'dired-narrow-fuzzy))
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
  (ivy-set-prompt 'counsel-fd counsel-prompt-function)
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
(spacemacs/set-leader-keys "3" 'yq/find-org|gtd)
(spacemacs/set-leader-keys "4" 'yq/find-org|project)
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
      -- Press \"fn\" to show file actions
      key code 63
    end tell'" file))))
(defun yq/open-with-alfred ()
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (yq/open-with-call-alfred-osascript (dired-get-filename nil t))
    (and (file-exists-p buffer-file-name) (yq/open-with-call-alfred-osascript buffer-file-name))))
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

(use-package prescient
  :straight t
  :commands (prescient-persist-mode)
  :init (prescient-persist-mode))

(use-package ivy-prescient
  :straight t
  :commands (ivy-prescient-mode)
  :init
  ;; (add-to-list 'ivy-prescient-excluded-commands 'counsel-fd)
  (ivy-prescient-mode))

(use-package company-prescient
  :straight t
  :commands (company-prescient-mode)
  :init (company-prescient-mode))

(yq/update-evil-emacs-state-modes 'ibuffer-mode)
;; (push 'ibuffer-mode evil-insert-state-modes)
;; (define-key ibuffer-mode-map "j" 'ibuffer-forward-line)
;; (define-key ibuffer-mode-map "k" 'ibuffer-backward-line)
(add-to-list 'ibuffer-never-show-predicates "^\\*Ibuffer")
(add-to-list 'ibuffer-never-show-predicates "^\\*Straight")
(add-to-list 'ibuffer-never-show-predicates "^\\*scratch")
(add-to-list 'ibuffer-never-show-predicates "^\\*Messages")
(add-to-list 'ibuffer-never-show-predicates "^\\*Warnings")
(add-to-list 'ibuffer-never-show-predicates "^\\*:Buffers:")
(add-to-list 'ibuffer-never-show-predicates "^\\*mu4e")
(add-to-list 'ibuffer-never-show-predicates "^\\*Help")

;; (use-package session
;;   :straight t
;;   :init
;;   (setq session-save-file (expand-file-name "~/.emacs.d/.session"))
;;   (add-hook 'after-init-hook 'session-initialize))

;; (use-package ivy-filthy-rich
;;   :straight (:host github :repo "casouri/ivy-filthy-rich")
;;   :commands (ivy-filthy-rich-mode)
;;   :init (ivy-filthy-rich-mode))

;; (use-package deadgrep
;;   :straight t
;;   :commands (deadgrep)
;;   :config
;;   (evil-define-key 'normal deadgrep-mode-map (kbd "RET") 'deadgrep-visit-result)
;;   (evil-define-key 'normal deadgrep-mode-map (kbd "TAB") 'deadgrep-toggle-file-results))

(use-package avy
  :straight t
  :init
  (define-key evil-normal-state-map "si" 'avy-goto-subword-1)
  (define-key evil-normal-state-map "sI" 'avy-goto-char-2))

(use-package rg
  :straight t
  :commands (rg rg-dwim rg-literal rg-project))

(use-package ace-link
  :straight t
  :commands (ace-link)
  :init
  (evil-define-key 'normal helpful-mode-map "o" 'ace-link-help)
  (ace-link-setup-default))