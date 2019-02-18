(setq
 ;; user-mail-address "namy.19@gmail.com"
 user-mail-address "hi@yqrashawn.com"
 ;; smtpmail-starttls-credentials '(("smtp.gmail.com" "587" nil nil))
 smtpmail-starttls-credentials '(("smtp.zoho.com" "587" nil nil))
 smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
 smtpmail-stream-type 'starttls
 ;; smtpmail-default-smtp-server "smtp.gmail.com"
 ;; smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-default-smtp-server "smtp.zoho.com"
 smtpmail-smtp-server "smtp.zoho.com"
 smtpmail-smtp-service 587
 smtpmail-debug-info t
 starttls-extra-arguments nil
 starttls-gnutls-program "/usr/local/bin/gnutls-cli"
 starttls-extra-arguments nil
 starttls-use-gnutls t)

(use-package mu4e
  :straight (:host github :repo "emacsmirror/mu4e" :branch "master"
                   :files ("mu4e/*" ("mu4e/mu4e-meta.el.in" . "mu4e-meta.el")))
  :custom
  (mu4e-attachment-dir "~/Downloads")
  (mu4e-compose-signature-auto-include nil)
  (mu4e-drafts-folder "/gmail/Drafts")
  (mu4e-maildir "~/Maildir")
  (mu4e-get-mail-command "proxychains4 -f /etc/proxychains.conf mbsync gmail")
  (mu4e-completing-read-function 'completing-read)
  (mu4e-use-fancy-chars 't)
  (mu4e-view-show-images 't)
  (message-kill-buffer-on-exit 't)
  (mu4e-maildir-shortcuts
   '(("/gmail/Inbox" . ?i)
     ("/gmail/[Gmail]/Starred" . ?s)
     ;; ("/gmail/[Gmail]/All Mail" . ?a)
     ("/gmail/[Gmail]/Sent Mail" . ?S)
     ("/gmail/[Gmail]/Trash" . ?d)
     ("/gmail/[Gmail]/Drafts" . ?D)
     ("/gmail/Starred" . ?S)))
  (mu4e-sent-folder "/gmail/[Gmail]/Sent Mail")
  (mu4e-drafts-folder "/gmail/[Gmail]/Drafts")
  (mu4e-refile-folder "/gmail/[Gmail]/Misc")
  (mu4e-trash-folder "/gmail/[Gmail]/Trash")
  (mu4e-change-filenames-when-moving t)
  (mu4e-update-interval nil)
  (mu4e-use-fancy-chars t)
  (mu4e-view-show-addresses t)
  (mu4e-view-show-images t)
  (mu4e-confirm-quit nil)
  (smtpmail-queue-mail nil)
  (smtpmail-queue-dir "~/Maildir/queue/cur")
  :init
  (setq mu4e-html2text-command 'mu4e-shr2text
        shr-color-visible-luminance-min 60
        shr-color-visible-distance-min 5)
  ;; (setq mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t plain")
  ;; (setq mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t org")
  (yq/update-evil-emacs-state-modes 'mu4e-headers-mode)
  (yq/update-evil-emacs-state-modes 'mu4e-view-mode)
  (yq/update-evil-emacs-state-modes 'mu4e-main-mode)
  (global-set-key (kbd "C-x m") 'mu4e-compose-new)
  (spacemacs/set-leader-keys "1" 'mu4e)
  ;; (spacemacs/set-leader-keys "2" (lambda () (interactive) ( mu4e~headers-jump-to-maildir "/gmail/INBOX")))
  ;;   (defun yq/mu4e~proc-remove (docid msgid)
  ;;     "Remove message identified by docid.
  ;; The results are reporter through either (:update ... ) or (:error)
  ;; sexp, which are handled my `mu4e-error-func', respectively."
  ;;     (mu4e~proc-send-command "cmd:remove docid:%d" docid)
  ;;     (mu4e~proc-send-command "cmd:remove msgid:%s" msgid))
  :config
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name  "Unread filtered"
                :query "flag:unread and not maildir:/INBOX.gitlab and not maildir:/INBOX.github and not maildir:/INBOX.mailinglist.something-user and not maildir:/INBOX.mailinglist.other-user"
                :key ?n))

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

  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode))
  ;; (evil-define-key 'normal mu4e-headers-mode-map "q" 'mu4e~headers-quit-buffer)
  ;; ;; (evil-define-key 'normal mu4e-headers-mode-map "q" 'yq/kill-this-buffer)
  ;; (evil-define-key 'normal mu4e-headers-mode-map "gr" 'mu4e-headers-rerun-search)

  ;;
  ;; (evil-define-key 'normal mu4e-headers-mode-map (kbd "<RET>") 'mu4e-headers-view-message)
  ;; (evil-define-key 'normal mu4e-headers-mode-map (kbd "l") 'mu4e-headers-view-message)
  ;; (evil-define-key 'normal mu4e-main-mode-map "u" 'mu4e-update-index)
  ;; (evil-define-key 'normal mu4e-main-mode-map "f" 'mu4e-headers-search)
  ;; (evil-define-key 'normal mu4e-main-mode-map "j" 'mu4e~headers-jump-to-maildir)
  ;; (evil-define-key 'normal mu4e-main-mode-map (kbd "C-j") 'next-line)
  ;; (evil-define-key 'normal mu4e-main-mode-map (kbd "C-k") 'previous-line)
  ;; (evil-define-key 'normal mu4e-main-mode-map "q" 'quit-window)
  ;; (evil-define-key 'normal mu4e-main-mode-map "b" 'mu4e-headers-search-bookmark)
  ;; (evil-define-key 'normal mu4e-view-mode-map (kbd "q") 'mu4e~view-quit-buffer)

(use-package mu4e-alert
  :straight t
  :after mu4e
  :init
  (mu4e-alert-enable-notifications)
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-set-default-style 'notifier))

;; (use-package mu4e-conversation
;;   :straight t
;;   :after mu4e
;;   :config
;;   (global-mu4e-conversation-mode))

(use-package evil-mu4e
  :straight t
  :after mu4e
  :config
  (evil-define-key 'normal mu4e-view-mode-map (kbd "<RET>") 'mu4e~view-browse-url-from-binding)
  (evil-define-key 'normal mu4e-main-mode-map "U" 'mu4e-update-index)
  (evil-define-key 'normal mu4e-main-mode-map "j" 'mu4e~headers-jump-to-maildir)
  (evil-define-key 'normal mu4e-view-mode-map "o" 'ace-link-mu4e)
  (evil-define-key 'normal mu4e-view-mode-map "go" 'mu4e-view-open-attachment))