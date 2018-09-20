(use-package mu4e
  :straight t
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
     ("/gmail/[Gmail]/All Mail" . ?a)
     ("/gmail/[Gmail]/Sent Mail" . ?S)
     ("/gmail/[Gmail]/Trash" . ?d)
     ("/gmail/[Gmail]/Drafts" . ?D)
     ("/gmail/Starred" . ?S)))
  (mu4e-sent-folder "/gmail/[Gmail]/Sent Mail")
  (mu4e-drafts-folder "/gmail/[Gmail]/Drafts")
  (mu4e-refile-folder "/gmail/[Gmail]/Starred")
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
  (spacemacs/set-leader-keys "2" (lambda () (interactive) ( mu4e~headers-jump-to-maildir "/gmail/INBOX")))
  :config
  (defvar mu4e-marks
    '((refile
       :char ("r" . "▶")
       :prompt "refile"
       :dyn-target (lambda (target msg) (mu4e-get-refile-folder msg))
       :action (lambda (docid msg target) (mu4e~proc-move docid
                                                          (mu4e~mark-check-target target) "-N")))
      (delete
       :char ("D" . "❌")
       :prompt "Delete"
       :show-target (lambda (target) "delete")
       :action (lambda (docid msg target)
                 (print (mu4e-msg-field msg :message-id))
                 (yq/mu4e~proc-remove (mu4e-msg-field msg :message-id))))
      (flag
       :char ("+" . "✚")
       :prompt "+flag"
       :show-target (lambda (target) "flag")
       :action (lambda (docid msg target) (mu4e~proc-move docid nil "+F-u-N")))
      (move
       :char ("m" . "▷")
       :prompt "move"
       :ask-target  mu4e~mark-get-move-target
       :action (lambda (docid msg target) (mu4e~proc-move docid
                                                          (mu4e~mark-check-target target) "-N")))
      (read
       :char    ("!" . "◼")
       :prompt "!read"
       :show-target (lambda (target) "read")
       :action (lambda (docid msg target) (mu4e~proc-move docid nil "+S-u-N")))
      (trash
       :char ("d" . "▼")
       :prompt "dtrash"
       :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
       :action (lambda (docid msg target) (mu4e~proc-move docid
                                                          (mu4e~mark-check-target target) "+T-N")))
      (unflag
       :char    ("-" . "➖")
       :prompt "-unflag"
       :show-target (lambda (target) "unflag")
       :action (lambda (docid msg target) (mu4e~proc-move docid nil "-F-N")))
      (untrash
       :char   ("=" . "▲")
       :prompt "=untrash"
       :show-target (lambda (target) "untrash")
       :action (lambda (docid msg target) (mu4e~proc-move docid nil "-T")))
      (unread
       :char    ("?" . "◻")
       :prompt "?unread"
       :show-target (lambda (target) "unread")
       :action (lambda (docid msg target) (mu4e~proc-move docid nil "-S+u-N")))
      (unmark
       :char  " "
       :prompt "unmark"
       :action (mu4e-error "No action for unmarking"))
      (action
       :char ( "a" . "◯")
       :prompt "action"
       :ask-target  (lambda () (mu4e-read-option "Action: " mu4e-headers-actions))
       :action  (lambda (docid msg actionfunc)
                  (save-excursion
                    (when (mu4e~headers-goto-docid docid)
                      (mu4e-headers-action actionfunc)))))
      (something
       :char  ("*" . "✱")
       :prompt "*something"
       :action (mu4e-error "No action for deferred mark")))

    "The list of all the possible marks.
This is an alist mapping mark symbols to their properties.  The
properties are:
  :char (string) or (basic . fancy) The character to display in
    the headers view. Either a single-character string, or a
    dotted-pair cons cell where the second item will be used if
    `mu4e-use-fancy-chars' is `t', otherwise we'll use
    the first one. It can also be a plain string for backwards
    compatibility since we didn't always support
    `mu4e-use-fancy-chars' here.
  :prompt (string) The prompt to use when asking for marks (used for
     example when marking a whole thread)
  :ask-target (function returning a string) Get the target.  This
     function run once per bulk-operation, and thus is suitable
     for user-interaction.  If nil, the target is nil.
  :dyn-target (function from (TARGET MSG) to string).  Compute
     the dynamic target.  This is run once per message, which is
     passed as MSG.  The default is to just return the target.
  :show-target (function from TARGET to string) How to display
     the target.
  :action (function taking (DOCID MSG TARGET)).  The action to
     apply on the message.")

  (defun yq/mu4e~proc-remove (msgid)
    "Remove message identified by docid.
The results are reporter through either (:update ... ) or (:error)
sexp, which are handled my `mu4e-error-func', respectively."
    (mu4e~proc-send-command "cmd:remove msgid:%s" msgid))
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

  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
  (evil-define-key 'normal mu4e-headers-mode-map "f" 'mu4e-headers-search)
  (evil-define-key 'normal mu4e-headers-mode-map "D" 'mu4e-headers-mark-for-trash)
  (evil-define-key 'normal mu4e-headers-mode-map "d" 'mu4e-headers-mark-for-delete)
  (evil-define-key 'normal mu4e-headers-mode-map "r" 'mu4e-headers-mark-for-refile)
  (evil-define-key 'normal mu4e-headers-mode-map "R" 'mu4e-headers-mark-for-move)
  (evil-define-key 'normal mu4e-headers-mode-map "u" 'mu4e-headers-mark-for-unmark)
  (evil-define-key 'normal mu4e-headers-mode-map "q" 'mu4e~headers-quit-buffer)
  ;; (evil-define-key 'normal mu4e-headers-mode-map "q" 'yq/kill-this-buffer)
  (evil-define-key 'normal mu4e-headers-mode-map "x" 'mu4e-mark-execute-all)
  (evil-define-key 'normal mu4e-headers-mode-map "m" 'mu4e-headers-mark-for-read)
  (evil-define-key 'normal mu4e-headers-mode-map "M" 'mu4e-headers-mark-for-unread)

  (evil-define-key 'normal mu4e-view-mode-map "f" 'mu4e-headers-search)
  (evil-define-key 'normal mu4e-view-mode-map "o" 'ace-link-mu4e)
  (evil-define-key 'normal mu4e-view-mode-map "a" 'mu4e-view-action)
  (evil-define-key 'normal mu4e-view-mode-map "D" 'mu4e-view-mark-for-trash)
  (evil-define-key 'normal mu4e-view-mode-map "d" 'mu4e-view-mark-for-delete)
  (evil-define-key 'normal mu4e-view-mode-map "r" 'mu4e-view-mark-for-refile)
  (evil-define-key 'normal mu4e-view-mode-map "R" 'mu4e-view-mark-for-move)
  (evil-define-key 'normal mu4e-view-mode-map "u" 'mu4e-view-mark-for-unmark)
  (evil-define-key 'normal mu4e-view-mode-map (kbd "<RET>") 'mu4e~view-browse-url-from-binding)
  (evil-define-key 'normal mu4e-headers-mode-map (kbd "<RET>") 'mu4e-headers-view-message)
  (evil-define-key 'normal mu4e-headers-mode-map (kbd "l") 'mu4e-headers-view-message)
  (evil-define-key 'normal mu4e-main-mode-map "u" 'mu4e-update-index)
  (evil-define-key 'normal mu4e-main-mode-map "f" 'mu4e-headers-search)
  (evil-define-key 'normal mu4e-main-mode-map "j" 'mu4e~headers-jump-to-maildir)
  (evil-define-key 'normal mu4e-main-mode-map (kbd "C-j") 'next-line)
  (evil-define-key 'normal mu4e-main-mode-map (kbd "C-k") 'previous-line)
  (evil-define-key 'normal mu4e-main-mode-map "q" 'quit-window)
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
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-set-default-style 'notifier))
