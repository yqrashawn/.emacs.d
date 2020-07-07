(use-package slack
  :straight t
  :commands (slack-start)
  :custom
  (slack-typing-visibility 'never)
  (slack-buffer-emojify t)
  (slack-prefer-current-team t)
  :init
  (defun +slack-change-current-team (teamname)
    (let* ((alist (mapcar
                   #'(lambda (team)
                       (cons (slack-team-name team)
                             (oref team token)))
                   (hash-table-values slack-teams-by-token)))
           (team (slack-team-find-by-token
                  (cdr-safe
                   (cl-assoc teamname alist :test #'string=)))))
      (setq slack-current-team team)
      (message "Set slack-current-team to %s" (or (and team (oref team name)) "nil"))
      (if team (slack-team-connect team))))
  :config
  (slack-register-team
   :name "Conflux"
   :token (auth-source-pick-first-password
           :host "conflux-world.slack.com"
           :login "yuxiao@conflux-chain.org")
   :subscribed-channels '((react-ui sirius conflux-portal design-system new-efficient-tool wearefamily tech-share jenkins general random)))
  (+slack-change-current-team "Conflux")
  (+slack-change-current-team "10i.cc")
  ;; (slack-register-team
  ;;  :name "Conflux Global"
  ;;  :token (auth-source-pick-first-password
  ;;          :host "confluxchain.slack.com"
  ;;          :login "yuxiao@conflux-chain.org")
  ;;  :subscribed-channels '((react-ui)))

  (slack-register-team
   :name "10i.cc"
   :token (auth-source-pick-first-password
           :host "10icc.slack.com"
           :login "namy.19@gmail.com")
   :subscribed-channels '((general random)))

  (evil-define-key 'insert lui-mode-map
    (kbd "RET") #'lui-send-input
    (kbd "TAB") #'lui-next-button-or-complete
    (kbd "<backtab>") #'lui-previous-button
    (kbd "<S-tab>") #'lui-previous-button
    (kbd "M-p") #'lui-previous-input
    (kbd "M-n") #'lui-next-input
    (kbd "C-c C-u") #'lui-kill-to-beginning-of-line
    (kbd "C-c C-i") #'lui-fool-toggle-display)

  (evil-define-key 'normal slack-info-mode-map
    ",u" 'slack-room-update-messages)

  (evil-define-key 'normal slack-pinned-items-buffer-mode-map
    "q" 'yq/kill-this-buffer)

  (evil-define-key 'normal slack-message-buffer-mode-map
    ",c" 'yq/kill-buffer-and-window
    ;; ",ra" 'slack-message-add-reaction
    ;; ",rr" 'slack-message-remove-reaction
    ;; ",rs" 'slack-message-show-reaction-users
    ",pl" 'slack-room-pins-list
    ",pa" 'slack-message-pins-add
    ",pr" 'slack-message-pins-remove
    ",mm" 'slack-message-write-another-buffer
    ",me" 'slack-message-edit
    ",md" 'slack-message-delete
    ",u" 'slack-room-update-messages
    ",2" 'slack-message-embed-mention
    ",3" 'slack-message-embed-channel
    "\C-n" 'slack-buffer-goto-next-message
    "\C-p" 'slack-buffer-goto-prev-message)

  (evil-define-key 'normal slack-edit-message-mode-map
    ",k" 'slack-message-cancel-edit
    ",s" 'slack-message-send-from-buffer
    ",2" 'slack-message-embed-mention
    ",3" 'slack-message-embed-channel)
  :config/el-patch
  (defun slack-message-cancel-edit ()
    (interactive)
    (let ((buffer (slack-buffer-buffer slack-current-buffer)))
      (with-current-buffer buffer
        (erase-buffer)
        (if (> (count-windows) 1) (el-patch-swap (delete-window)
                                                 (yq/kill-this-buffer)))))))

(use-package alert
  :straight t
  :commands (alert)
  :custom
  (alert-default-style 'osx-notifier)
  :config/el-patch
  (defun alert-osx-notifier-notify (info)
    (apply #'call-process "osascript" nil nil nil "-e"
           (list (el-patch-swap
                   (format "display notification %S with title %S"
                           (alert-encode-string (plist-get info :message))
                           (alert-encode-string (plist-get info :title)))
                   (format (if (eq (plist-get info :category) 'slack)
                               "display notification %S with title %S sound name \"Tink\""
                               "display notification %S with title %S")
                           (alert-encode-string (plist-get info :message))
                           (alert-encode-string (plist-get info :title))))))
    (alert-message-notify info)))