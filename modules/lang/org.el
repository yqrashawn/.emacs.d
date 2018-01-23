(use-package org
  :straight t
  :config
  (setq org-id-method 'org)
  (setq org-clock-idle-time 10)
  (setq org-confirm-babel-evaluate nil)
  (setq org-tag-alist '((:startgroup . nil)
                        ("work" . ?w) ("home" . ?h)
                        (:endgroup . nil)))
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
  (setq org-todo-keyword-faces
        (quote (("TODO"      :foreground "lightblue"    :weight bold)
                ("NEXT"      :foreground "red"          :weight bold)
                ("STARTED"   :foreground "red"          :weight bold)
                ("DONE"      :foreground "forest green" :weight bold)
                ("WAITING"   :foreground "orange"       :weight bold)
                ("TEAM"      :foreground "orange"       :weight bold)
                ("SOMEDAY"   :foreground "magenta"      :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)
                ("QUOTE"     :foreground "red"          :weight bold)
                ("QUOTED"    :foreground "magenta"      :weight bold)
                ("APPROVED"  :foreground "forest green" :weight bold)
                ("EXPIRED"   :foreground "forest green" :weight bold)
                ("REJECTED"  :foreground "forest green" :weight bold)
                ("OPEN"      :foreground "blue"         :weight bold)
                ("CLOSED"    :foreground "forest green" :weight bold)
                ("PHONE"     :foreground "forest green" :weight bold))))
  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED")
                ("ARCHIVE" . t)
                ("WAITING")
                ("WAITING" . t)
                (done)
                ("WAITING")
                ("TODO")
                ("WAITING")
                ("CANCELLED")
                ("NEXT")
                ("WAITING")
                ("STARTED")
                ("WAITING")
                ("DONE")
                ("WAITING")
                ("CANCELLED"))))

  (setq org-log-note-headings '((done . "CLOSING NOTE %t")
                                (state . "State %-12s from %-12S %T")
                                (note . "Note taken on %t")
                                (reschedule . "Rescheduled from %S on %t")
                                (delschedule . "Not scheduled, was %S on %t")
                                (redeadline . "New deadline from %S on %t")
                                (deldeadline . "Removed deadline, was %S on %t")
                                (refile . "Refiled on %t")
                                (clock-out . "")))
  (setq org-log-note-headings '((done . "CLOSING DONE NOTE %t")
                                (state . "State %-12s from %-12S %t")
                                (note . "Note taken on %t")
                                (reschedule . "Rescheduled from %S on %t")
                                (delschedule . "Not scheduled, was %S on %t")
                                (redeadline . "New deadline from %S on %t")
                                (deldeadline . "Removed deadline, was %S on %t")
                                (refile . "Refiled on %t")
                                (clock-out . "")))
  (setq org-todo-keywords (quote)
        ((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "WAITING(w@/!)" "SOMEDAY(S!)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")))
  ;; https://github.com/novoid/dot-emacs/blob/28c146f785c1d87dc821514e8448e3dfe82e56ce/config.org
  (setq org-log-done (quote time))
  (setq org-log-into-drawer t)
  (setq org-log-redeadline (quote note));; record when the deadline date of a tasks is modified
  (setq org-log-reschedule (quote time))
  (setq org-return-follows-link t)
  (setq org-remove-highlights-with-change nil)
  (setq org-read-date-prefer-future 'time)
  (setq org-list-demote-modify-bullet (quote (("+" . "-")
                                              ("*" . "-")
                                              ("1." . "-")
                                              ("1)" . "-"))))
  (setq org-adapt-indentation nil);; do not indent drawers/body according to heading level
  (setq org-startup-indented t)

  (setq org-enforce-todo-dependencies t)
  (setq org-insert-heading-respect-content nil)
  (setq org-reverse-note-order nil)
  (setq org-deadline-warning-days 1)
  (setq org-blank-before-new-entry '((heading . t))
        (plain-list-item . nil))
  (setq org-todo-repeat-to-state "NEXT")
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)
  (setq org-hierarchical-todo-statistics t)

  (evil-define-key 'insert org-mode-map (kbd "C-y") 'org-yank)
  (setq org-yank-adjusted-subtrees t)

  (setq org-show-context-detail
        '((agenda . lineage) ;; instead of "local"
          (bookmark-jump . lineage)
          (isearch . lineage)
          (default . ancestors)))
  (setq org-directory "~/Dropbox/ORG")
  (setq org-default-notes-file '("~/Dropbox/ORG/notes.org"))
  (setq org-capture-templates
        '(("s" "Some day" entry)
          (file+olp "~/Dropbox/ORG/notes.org" "notes" "some day")
          "*** TODO %? %^L %^G\n%U"
          ("l" "Capture from the Internet with link" entry)
          (file+olp "~/Dropbox/ORG/notes.org" "notes" "read")
          "*** TODO %? %^L %^G\n%U"
          ("b" "Brain" plain (function org-brain-goto-end))
          "* %i%?\n"
          ("n" "notes" entry)
          (file+olp "~/Dropbox/ORG/notes.org" "notes" "note")
          "*** %?\n   %U"
          ("c" "code snipptes" entry)
          (file+olp "~/Dropbox/ORG/snipptes.org" "snipptes")
          "**** %?\n%U"
          ("f" "file TODOs" entry)
          (file "~/Dropbox/ORG/gtd.org")
          "* TODO %? \n %a\n%U"
          ("t" "TODOs" entry)
          (file+olp "~/Dropbox/ORG/gtd.org" "misc")
          "* TODO %? \n%U"))
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (setq org-log-note-clock-out t)
  (setq org-clock-idle-time 10)
  (defun my-handle-tsfile-link (querystring)
    (let ((querystring)
          (if (s-contains-p "/" querystring)
              (f-filename querystring)
            (s-replace " " ".*" querystring)))
      (message (concat "DEBUG1: querystring: " querystring))
      (message (concat "DEBUG2: "
                       "fd \""
                       querystring
                       "\" "
                       (concat blog-root blog-file-pattern)))
      ;; get a list of hits
      (let ((queryresults (split-string)
                          (s-trim
                           (shell-command-to-string)
                           (concat
                            "fd \""
                            querystring
                            "\" "
                            (concat blog-root blog-file-pattern)))
                          "\n" t))
        (message (concat "DEBUG3: queryresults: " (car queryresults)))
        ;; check length of list (number of lines)
        (cond)
        ((= 0 (length queryresults)
            ;; edge case: empty query result
            (message "Sorry, no results found for query: %s" querystring)))
        (t
         (with-temp-buffer
           (spacemacs//open-in-external-app (if (= 1 (length queryresults))
                                                (car queryresults)
                                              (completing-read "Choose: " queryresults))))))))
  (org-link-set-parameters
   "tsfile"
   :follow (lambda (path) (my-handle-tsfile-link path))
   :help-echo "Opens the linked file with your default application")
  (defun my-jump-to-lazyblorg-heading-according-to-URL-in-clipboard ()
    "Retrieves an URL from the clipboard, gets its Org-mode source,
    extracts the ID of the article and jumps to its Org-mode heading"
    (interactive)
    (let (
          ;; Getting URL from the clipboard. Since it may contain
          ;; some text properties we are using substring-no-properties
          ;; function
          (url (substring-no-properties (current-kill 0)))
          ;; This is a check string: if the URL in the clipboard
          ;; doesn't start with this, an error message is shown
          (domain "yqrashawn.com"))

      ;; Check if URL string is from my domain (all other strings do
      ;; not make any sense here)
      (if (string-match (upcase domain) (upcase url))
          ;; Retrieving content by URL into new buffer asynchronously
          (url-retrieve url
                        ;; call this lambda function when URL content is retrieved
                        (lambda (status)
                          ;; Extrating and preparing the ID
                          (let* ()
                            ;; Limit the ID search to the top 1000 characters of the buffer
                            (pageheader (buffer-substring 1 1000))
                            ;; Start index of the id
                            (start (string-match "<meta name=\"orgmode-id\" content=\"" pageheader))
                            ;; End index of the id
                            (end (string-match "\" />" pageheader start))
                            ;; Amount of characters to skip for the openning tag
                            (chars-to-skip (length "<meta name=\"orgmode-id\" content=\""))
                            ;; Extract ID
                            (lazyblorg-id (if (and start end (< start end))
                                              ;; ... extract it and return.
                                              (substring pageheader (+ start chars-to-skip) end)
                                            nil))

                            (message (concat "Looking for id:" lazyblorg-id))
                            (org-open-link-from-string (concat "ID:" lazyblorg-id)))))



        (message (concat "Sorry: the URL \"" (substring url 0 (length domain)) "...\" doesn't contain \"" domain "\". Aborting."))))))

(use-package org-projectile
  :straight t
  :config
  (setq org-projectile-capture-template "* TODO %? %^G\n%U"))
