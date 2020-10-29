(setq vc-handled-backends '(Git))
(remove-hook 'find-file-hooks 'vc-find-file-hook)

(use-package git-commit
  :straight t
  :config
  ;; https://emacs.stackexchange.com/a/41405
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))

(use-package magit-popup
  :straight t)

(use-package transient
  :straight t
  :defer t
  :custom
  (transient-default-level 7))

(use-package magit
  :straight t
  :custom
  (magit-log-margin '(t age-abbreviated magit-log-margin-width t 18))
  ;; (magit-log-margin '(t "%Y-%m-%d %H:%M "  magit-log-margin-width t 18))
  (magit-diff-refine-hunk t)
  (magit-section-initial-visibility-alist '((stashes . hide)
                                            (untracked . show)
                                            (staged . show)))
  :init
  ;; (setq magit-bury-buffer-function (lambda (&optional kill-buffer) (magit-restore-window-configuration t)))
  (setq magit-bury-buffer-function #'magit-mode-quit-window)
  ;; (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-save-repository-buffers 'dontask)
  (setq magit-commit-ask-to-stage t)
  (setq magit-fetch-modules-jobs 10)
  (setq magit-push-arguments '("--set-upstream"))
  (setq magit-repository-directories '(("~/.emacs.d" . 0)
                                       ;; ("~/.emacs.d/straight/repos/" . 1)
                                       ("~/workspace/HOME/" . 1)
                                       ("~/workspace/OFFICE/" . 1)))
  (setq magit-blame-echo-style 'margin)
  (setq magit-diff-refine-hunk 'all)
  (define-key yq-s-map "u" #'magit-dispatch-popup)
  (spacemacs/set-leader-keys "g" nil)
  (spacemacs/set-leader-keys "gl" #'magit-dispatch-popup)
  (spacemacs/set-leader-keys "gc" #'magit-checkout)
  (spacemacs/set-leader-keys "gf" #'magit-file-dispatch)
  (spacemacs/set-leader-keys "gg" #'magit-list-repositories)
  (spacemacs/set-leader-keys "gs" #'magit-status)
  (spacemacs/set-leader-keys "gS" (defl (setq current-prefix-arg '(1))
                                    (call-interactively #'magit-status)))
  :config
  (transient-define-argument magit-merge:--strategy-option ()
    :description "Strategy Option"
    :class 'transient-option
    ;; key for merge and rebase: "-s"
    ;; key for cherry-pick and revert: "=s"
    ;; shortarg for merge and rebase: "-s"
    ;; shortarg for cherry-pick and revert: none
    :key "-X"
    :argument "--strategy-option="
    :choices '("ours" "theirs" "patience" "subtree" "renormalize" "no-renormalize" "no-renames" "diff-algorithim=patience" "diff-algorithim=minimal" "diff-algorithim=histogram" "diff-algorithim=myers"))
  (transient-append-suffix 'magit-rebase "-s" '("-X" magit-merge:--strategy-option))
  (magit-wip-mode 1)
  (magit-auto-revert-mode 1)
  (add-to-list 'magit-no-confirm 'stage-all-changes)
  (defun +magit-submodule-pull-all ()
    (interactive)
    (async-shell-command "git submodule foreach git fetch --all && git submodule foreach git merge origin/master"))
  (magit-define-popup-action 'magit-submodule-popup ?U "pull all submodules" '+magit-submodule-pull-all)
  ;; (add-to-list 'magit-diff-arguments "--minimal")
  ;; (add-to-list 'magit-diff-section-arguments "--minimal")
  (evil-define-key 'normal magit-mode-map (kbd "<tab>") 'magit-section-toggle)
  ;; add submodule in magit-status buffer
  ;; https://emacs.stackexchange.com/a/39009/14357
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-overview
                          'magit-insert-unpulled-from-upstream)
  ;; https://emacs-pe.github.io/2015/06/30/magit-github-pr/
  (defun marsam/add-pull-request-refs (&optional remote local-ns)
    "Set pull requests refs from a REMOTE with LOCAL-NS namespace into Git config."
    (interactive (let* ((remote (magit-read-remote "Fetch remote"))
                        (local-ns (read-string "local namespace: " (format "%s/pr" remote))))
                   (list remote local-ns)))
    (and (not (magit-get-boolean "core" "disableprref"))
         (let* ((remote (or remote "origin"))
                (local-ns (if (or (null local-ns) (string= "" local-ns)) (format "%s/pr" remote) local-ns))
                (pr-refs (format "+refs/pull/*/head:refs/remotes/%s/*" local-ns))
                (remote-fetch-refs (magit-get-all "remote" remote "fetch")))
           (and remote-fetch-refs
                (not (magit-get-boolean "remote" remote "disableprref"))
                (not (member pr-refs remote-fetch-refs))
                (string-match "github.com" (magit-get "remote" remote "url"))
                (magit-git-string "config" "--add" (format "remote.%s.fetch" remote) pr-refs)))))

  (defun +marsam/add-pull-request-refs ()
    (dolist (remote (magit-list-remotes))
            (marsam/add-pull-request-refs remote)))
  ;; (add-hook 'magit-mode-hook 'marsam/add-pull-request-refs)
  (add-hook 'magit-mode-hook '+marsam/add-pull-request-refs)
  (let ((maps (list magit-status-mode-map magit-log-mode-map magit-reflog-mode-map magit-diff-mode-map)))
    (dolist (map maps)
      (evil-define-key 'normal map "j" 'magit-next-line)
      (evil-define-key 'normal map "k" 'magit-previous-line)))
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-mode)
  (add-hook 'git-rebase-mode-hook 'turn-off-evil-snipe-mode)
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
  (add-hook 'git-rebase-mode-hook 'turn-off-evil-snipe-override-mode)
  (advice-add 'magit-process-filter :after #'+color-buffer)

  (defun my-magit-command (&rest _)
    (interactive)
    (setq this-command #'my-magit-command))
  (with-eval-after-load 'ivy
    (setf (alist-get 'my-magit-command ivy-re-builders-alist) #'ivy--regex-fuzzy))
  (add-function :before magit-completing-read-function #'my-magit-command)
  (add-to-list 'ivy-re-builders-alist '(magit-log-other . ivy--regex-fuzzy)))


(use-package evil-magit :straight t :after magit)

(use-package magit-cz
  :load-path "~/.emacs.d/modules"
  :after (magit git-commit transient))

(use-package abridge-diff
  :straight t
  :after magit
  :init (abridge-diff-mode 1))

(use-package with-editor
  :straight t
  :commands (with-editor-mode shell-command-with-editor-mode)
  :init
  (setq with-editor-emacsclient-executable "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")
  (shell-command-with-editor-mode)
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

(use-package forge
  :straight (:host github :repo "magit/forge")
  :after evil-magit
  :custom
  (forge-pull-notifications nil)
  :commands (forge-dispatch)
  :bind ((:map forge-issue-section-map
               ("C-c C-v" . forge-browse-topic))
         (:map forge-pullreq-section-map
               ("C-c C-v" . forge-browse-topic)))
  :config
  (add-to-list 'forge-owned-accounts '("yqrashawn" . (remote-name "own")))
  (add-to-list 'forge-alist '("917.bimsop.com" "917.bimsop.com/api/v1" "917.bimsop.com" forge-gogs-repository)))

(use-package diff-hl
  :straight t
  :commands (global-diff-hl-mode)
  :init (global-diff-hl-mode)
  :config
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package git-timemachine
  :straight t
  :commands (git-timemachine)
  :init
  (spacemacs/set-leader-keys
    "gt" 'git-timemachine)
  (evil-define-key 'normal git-timemachine-mode-map
    "n" 'git-timemachine-show-next-revision
    "p" 'git-timemachine-show-previous-revision
    "q" 'git-timemachine-quit
    "W" 'git-timemachine-kill-revision
    "c" 'git-timemachine-show-current-revision)
  (add-hook 'git-timemachine-mode-hook 'yq/fix-evil-state-bug))

(use-package smeargle
  :straight t
  :commands (smeargle))

(use-package magit-todos
  :straight t
  :after (magit)
  :custom
  (magit-todos-show-branch-list nil)
  :config
  (define-key magit-todos-section-map "j" nil)
  (define-key magit-todos-item-section-map (kbd "M-RET") #'magit-todos-peek-at-item))

(use-package vc-msg
  :straight t
  :commands (vc-msg-show)
  :custom
  (vc-msg-git-show-commit-function 'magit-show-commit)
  :init
  (spacemacs/set-leader-keys "gl" #'vc-msg-show))

;; https://emacs.stackexchange.com/questions/16469/how-to-merge-git-conflicts-in-emacs
(use-feature smerge-mode
  :defer t
  :diminish smerge-mode
  :config
  (spacemacs/set-leader-keys "gr" 'hydra-smerge/body)

  (with-eval-after-load 'hydra
   (defhydra hydra-smerge
                    (:color pink :hint nil :post (smerge-auto-leave))
                    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_j_ext       _b_ase               _<_: upper/base        _C_ombine
_k_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _K_ill current
^^           _a_ll                _R_efine               _w_save buffer
^^           _RET_: current       _e_diff
"
                    ("j" smerge-next)
                    ("k" smerge-prev)
                    ("b" smerge-keep-base)
                    ("u" smerge-keep-upper)
                    ("l" smerge-keep-lower)
                    ("a" smerge-keep-all)
                    ("RET" smerge-keep-current)
                    ("\C-m" smerge-keep-current)
                    ("<" smerge-diff-base-upper)
                    ("=" smerge-diff-upper-lower)
                    (">" smerge-diff-base-lower)
                    ("r" smerge-refine)
                    ("e" smerge-ediff)
                    ("C" smerge-combine-with-next)
                    ("R" smerge-resolve)
                    ("K" smerge-kill-current)
                    ("ZZ" (lambda ()
                            (interactive)
                            (save-buffer)
                            (bury-buffer))
                     "Save and bury buffer" :color blue)
                    ("w" (lambda ()
                           (interactive)
                           (save-buffer))
                     "Save buffer" :color blue)
                    ("q" nil "cancel" :color blue))))