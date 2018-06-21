(use-package mu4e
  :straight t
  :custom
  (mu4e-attachment-dir "~/Downloads")
  (mu4e-compose-signature-auto-include nil)
  (mu4e-drafts-folder "/gmail/Drafts")
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-maildir "~/Maildir")
  (mu4e-refile-folder "/gmail/Archive")
  (mu4e-sent-folder "/gmail/Sent Mail")
  (mu4e-get-mail-command "export https_proxy=http://127.0.0.1:6152;export http_proxy=http://127.0.0.1:6152;export HTTP_PROXY=http://127.0.0.1:6152:export HTTPs_PROXY=http://127.0.0.1:6152 & mbsync gmail")

  (mu4e-completing-read-function 'completing-read)
  (mu4e-use-fancy-chars 't)
  (mu4e-view-show-images 't)
  (message-kill-buffer-on-exit 't)
  (setq mu4e-maildir-shortcuts
   '(("/gmail/INBOX" . ?i)
     ("/gmail/[Gmail].All Mail" . ?a)
     ("/gmail/Trash" . ?d)
     ("/gmail/Drafts" . ?D)
     ("/gmail/[Gmail].Sent Mail" . ?s)
     ("/gmail/[Gmail].Starred" . ?S)))
  (mu4e-trash-folder "/gmail/Trash")
  (mu4e-change-filenames-when-moving t)
  (mu4e-update-interval 300)
  (mu4e-use-fancy-chars t)
  (mu4e-view-show-addresses t)
  (mu4e-view-show-images t)
  (mu4e-confirm-quit nil)
  (smtpmail-queue-mail nil)
  (smtpmail-queue-dir "~/Maildir/queue/cur")
  :init
  (setq mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t plain")
  (setq evil-emacs-state-modes (seq-remove
   (lambda (index)
     (not (and
           (not (eq index 'mu4e-view-mode))
           (and (not (eq index 'mu4e-headers-mode))
                (not (eq index 'mu4e-main-mode))))))
   evil-emacs-state-modes) )
  (global-set-key (kbd "C-x m") 'mu4e-compose-new)
  (spacemacs/set-leader-keys "1" 'mu4e)
  :config
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name  "Unread filtered"
                :query "flag:unread and not maildir:/INBOX.gitlab and not maildir:/INBOX.github and not maildir:/INBOX.mailinglist.something-user and not maildir:/INBOX.mailinglist.other-user"
                :key ?n)
               )
  (defun jcs-view-in-eww (msg)
    (eww-browse-url (concat "file://" (mu4e~write-body-to-html msg))))
  (add-to-list 'mu4e-view-actions '("eww view" . jcs-view-in-eww) t)
  (add-to-list 'mu4e-view-actions '("view in browser" . mu4e-action-view-in-browser))
  (setq mu4e-sent-messages-behavior 'delete)
  (add-hook 'mu4e-view-mode-hook #'visual-line-mode)
  (setq mail-user-agent 'mu4e-user-agent)
  (defvaralias 'mu4e-compose-signature 'message-signature)
  (setq mu4e-completing-read-function 'ivy-completing-read
        mu4e-confirm-quit nil)
  (setq mu4e-headers-show-threads nil) ; Use "P" to toggle threading
  ;; (setq mu4e-compose-signature (get-string-from-file "~/signature.txt"))

  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
  (evil-define-key 'normal mu4e-headers-mode-map "d" 'mu4e-headers-mark-for-trash)
  (evil-define-key 'normal mu4e-headers-mode-map "D" 'mu4e-headers-mark-for-delete)
  (evil-define-key 'normal mu4e-headers-mode-map "r" 'mu4e-headers-mark-for-read)
  (evil-define-key 'normal mu4e-headers-mode-map "R" 'mu4e-headers-mark-for-move)
  (evil-define-key 'normal mu4e-headers-mode-map "u" 'mu4e-headers-mark-for-unmark)
  (evil-define-key 'normal mu4e-headers-mode-map "q" 'mu4e~headers-quit-buffer)
  (evil-define-key 'normal mu4e-headers-mode-map "x" 'mu4e-mark-execute-all)

  (evil-define-key 'normal mu4e-view-mode-map "a" 'mu4e-view-action)
  (evil-define-key 'normal mu4e-view-mode-map "d" 'mu4e-view-mark-for-trash)
  (evil-define-key 'normal mu4e-view-mode-map "D" 'mu4e-view-mark-for-delete)
  (evil-define-key 'normal mu4e-view-mode-map "r" 'mu4e-view-mark-for-read)
  (evil-define-key 'normal mu4e-view-mode-map "R" 'mu4e-view-mark-for-move)
  (evil-define-key 'normal mu4e-view-mode-map "u" 'mu4e-view-mark-for-unmark)
  (evil-define-key 'normal mu4e-view-mode-map (kbd "<RET>") 'mu4e~view-browse-url-from-binding)
  (evil-define-key 'normal mu4e-headers-mode-map (kbd "<RET>") 'mu4e-headers-view-message)
  (evil-define-key 'normal mu4e-headers-mode-map (kbd "l") 'mu4e-headers-view-message)
  (evil-define-key 'normal mu4e-main-mode-map "j" 'mu4e~headers-jump-to-maildir)
  (evil-define-key 'normal mu4e-main-mode-map (kbd "C-j") 'next-line)
  (evil-define-key 'normal mu4e-main-mode-map (kbd "C-k") 'previous-line)
  (evil-define-key 'normal mu4e-main-mode-map "q" 'mu4e-quit)
  (evil-define-key 'normal mu4e-main-mode-map "b" 'mu4e-headers-search-bookmark)
  (evil-define-key 'normal mu4e-headers-mode-map (kbd "C-j") 'next-line)
  (evil-define-key 'normal mu4e-headers-mode-map (kbd "C-k") 'previous-line)
  (evil-define-key 'normal mu4e-headers-mode-map (kbd "J") (lambda ()
                                                             (interactive)
                                                             (mu4e-headers-mark-thread nil '(read))))
  (evil-define-key 'normal mu4e-view-mode-map (kbd "C-j") 'mu4e-view-headers-next)
  (evil-define-key 'normal mu4e-view-mode-map (kbd "C-k") 'mu4e-view-headers-prev)
  (evil-define-key 'normal mu4e-view-mode-map (kbd "q") 'next-line)
  (evil-define-key 'normal mu4e-view-mode-map (kbd "q") 'mu4e~view-quit-buffer)
  (evil-define-key 'normal mu4e-view-mode-map (kbd "J") (lambda ()
                                                             (interactive)
                                                             (mu4e-headers-mark-thread nil '(read)))))

(use-package mu4e-alert
  :straight t
  :after mu4e
  :init
  (mu4e-alert-enable-notifications)
  (mu4e-alert-enable-mode-line-display))