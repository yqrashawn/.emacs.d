(setq vc-handled-backends '(Git))
(remove-hook 'find-file-hooks 'vc-find-file-hook)
(defun git-get-current-file-relative-path ()
  (replace-regexp-in-string (concat "^" (file-name-as-directory default-directory))
                            ""
                            buffer-file-name))

(defun git-add-current-file ()
  "git add file of current buffer"
  (interactive)
  (let ((filename))
    (when buffer-file-name
      (setq filename (git-get-current-file-relative-path))
      (shell-command (concat "git add " filename))
      (message "DONE! git add %s" filename))))

(use-package git-commit
  :straight t)

(use-package magit-popup
  :straight t)

(use-package magit
  :straight t
  :init
  (magit-auto-revert-mode 1)
  (setq magit-bury-buffer-function #'magit-restore-window-configuration)
  ;; (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-save-repository-buffers 'dontask)
  (setq magit-commit-ask-to-stage t)
  (setq magit-fetch-modules-jobs 10)
  (setq magit-push-arguments '("--set-upstream"))
  (setq magit-repository-directories '(("~/.emacs.d" . 0)
                                       ;; ("~/.emacs.d/straight/repos/" . 1)
                                       ("~/workspace/HOME/" . 1)
                                       ("~/workspace/OFFICE/" . 1)
                                       ("~/workspace/THIRD/" . 1)
                                       ("~/.zprezto" . 0)))
  (setq magit-blame-echo-style 'margin)
  (setq magit-diff-refine-hunk 'all)
  :config
  (add-to-list 'magit-diff-arguments "--minimal")
  (add-to-list 'magit-diff-section-arguments "--minimal")
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

  (add-hook 'magit-mode-hook 'marsam/add-pull-request-refs)
  (let ((maps (list magit-status-mode-map magit-log-mode-map magit-reflog-mode-map magit-diff-mode-map)))
    (dolist (map maps)
      (evil-define-key 'normal map "j" 'magit-next-line)
      (evil-define-key 'normal map "k" 'magit-previous-line)))
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-mode)
  (add-hook 'git-rebase-mode-hook 'turn-off-evil-snipe-mode)
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
  (add-hook 'git-rebase-mode-hook 'turn-off-evil-snipe-override-mode)
  (spacemacs/set-leader-keys "g" nil)
  (spacemacs/set-leader-keys "gf" 'magit-file-popup)
  (spacemacs/set-leader-keys "gg" 'magit-list-repositories)
  (spacemacs/set-leader-keys "gs" 'magit-status))

(use-package evil-magit :straight t)

(use-package with-editor
  :straight t
  :commands (with-editor-mode shell-command-with-editor-mode)
  :hook (git-commit-mode-hook . with-editor-hook)
  :init
  (shell-command-with-editor-mode)
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  (spacemacs/set-leader-keys "hdK" 'describe-keymap))

(use-package magithub
  :straight t
  :disabled
  :after magit
  :config
  (magithub-feature-autoinject '(completion commit-browse))
  (setq magithub-clone-default-directory "~/workspace/THIRD/"
        magithub-dir spacemacs-cache-directory))


(straight-use-package 'closql)
(use-package forge
  :straight t
  :after magit
  :bind ((:map forge-issue-section-map
               ("C-c C-v" . forge-browse-topic))
         (:map forge-pullreq-section-map
               ("C-c C-v" . forge-browse-topic)))
  :init
  (define-key magit-mode-map "L" #'forge-dispatch)
  :config
  (add-to-list 'forge-alist '("917.bimsop.com" "917.bimsop.com/api/v1" "917.bimsop.com" forge-gogs-repository)))

;; (use-package magit-todos
;;   :straight t
;;   :after magit
;;   :init magit-todos-mode)

(use-package diff-hl
  :straight t
  :commands (global-diff-hl-mode)
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
