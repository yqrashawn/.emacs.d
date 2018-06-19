(defmacro spacemacs|org-emphasize (fname char)
  "Make function for setting the emphasis in org mode"
  `(defun ,fname () (interactive)
          (org-emphasize ,char)))
;; Insert key for org-mode and markdown a la C-h k
;; from SE endless http://emacs.stackexchange.com/questions/2206/i-want-to-have-the-kbd-tags-for-my-blog-written-in-org-mode/2208#2208
(defun spacemacs/insert-keybinding-org (key)
  "Ask for a key then insert its description.
  Will work on both org-mode and any mode that accepts plain html."
  (interactive "kType key sequence: ")
  (let* ((tag "@@html:<kbd>@@ %s @@html:</kbd>@@"))
    (if (null (equal key "\r"))
        (insert
         (format tag (help-key-description key nil)))
      (insert (format tag ""))
      (forward-char -8))))

(with-eval-after-load 'org-src
  (evil-define-key 'org-src-mode
    ",c" 'org-edit-src-exit
    ",a" 'org-edit-src-abort
    ",k" 'org-edit-src-abort))

(use-package org
  ;; :straight org-plus-contrib
  :ensure t
  :init
  ;; automatically change status of a heading to DONE when all children are done
  ;; src block have same indentation with #+BEGIN_SRC
  (setq org-edit-src-content-indentation 0)
  (setq org-clock-into-drawer "CLOCKING")
  (defun my-sparse-tree-with-tag-filter()
    "asks for a tag and generates sparse tree for all open tasks in current Org buffer
  that are associated with this tag"
    (interactive "*")
    (setq tag-for-filter
          (org-trim
           (org-icompleting-read "Tags: "
                                 'org-tags-completion-function
                                 nil nil nil 'org-tags-history)))
    (org-occur
     (concat "^\\*+ \\(NEXT\\|TODO\\|WAITING\\|STARTED\\) .+:"
             tag-for-filter
             ":")))
  (evil-define-key 'normal org-mode-map "ss" #'my-sparse-tree-with-tag-filter)

  ;; Add global evil-leader mappings. Used to access org-agenda
  ;; functionalities – and a few others commands – from any other mode.
  (spacemacs/set-leader-keys
    ;; org-agenda
    "ao#" 'org-agenda-list-stuck-projects
    "ao/" 'org-occur-in-agenda-files
    "aoa" 'org-agenda-list
    "aoc" 'org-capture
    "aoe" 'org-store-agenda-views
    "aoki" 'org-clock-in-last
    "aokj" 'org-clock-jump-to-current-clock
    "aoko" 'org-clock-out
    "aokr" 'org-resolve-clocks
    "aol" 'org-store-link
    "aom" 'org-tags-view
    "aoo" 'org-agenda
    "aos" 'org-search-view
    "aot" 'org-todo-list
    ;; SPC C- capture/colors
    "Cc" 'org-capture)
  (add-hook 'org-mode-hook 'hs-minor-mode)
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (define-key global-map "\C-cc" 'org-capture)
  (setq org-startup-with-inline-images t
        org-src-fontify-natively t
        ;; this is consistent with the value of
        ;; `helm-org-headings-max-depth'.
        org-imenu-depth 8)
  (setq org-clock-persist-file (concat spacemacs-cache-directory
                                       "org-clock-save.el")
        org-id-locations-file (concat spacemacs-cache-directory
                                      ".org-id-locations")
        org-publish-timestamp-directory (concat spacemacs-cache-directory
                                                ".org-timestamps/")
        org-directory "~/Dropbox/ORG" ;; needs to be defined for `org-default-notes-file'
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-log-done t
        org-startup-with-inline-images t
        org-image-actual-width nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        ;; this is consistent with the value of
        ;; `helm-org-headings-max-depth'.
        org-imenu-depth 8)
  :config
  (defun my-handle-tsfile-link (querystring)
    (let ((querystring
           (if (s-contains-p "/" querystring)
               (f-filename querystring)
             (s-replace " " ".*" querystring)
             )))
      (message (concat "DEBUG1: querystring: " querystring))
      (message (concat "DEBUG2: "
                       "fd \""
                       querystring
                       "\" "
                       (concat blog-root blog-file-pattern)))
      ;; get a list of hits
      (let ((queryresults (split-string
                           (s-trim
                            (shell-command-to-string
                             (concat
                              "fd \""
                              querystring
                              "\" "
                              (concat blog-root blog-file-pattern))))
                           "\n" t)))
        (message (concat "DEBUG3: queryresults: " (car queryresults)))
        ;; check length of list (number of lines)
        (cond ((= 0 (length queryresults))
               ;; edge case: empty query result
               (message "Sorry, no results found for query: %s" querystring))
              (t (with-temp-buffer
                   (spacemacs//open-in-external-app (if (= 1 (length queryresults))
                                                        (car queryresults)
                                                      (completing-read "Choose: " queryresults)))
                   ;; (insert (if (= 1 (length queryresults))
                   ;;             (car queryresults)
                   ;;           (completing-read "Choose: " queryresults)))
                   ;; (org-mode)
                   ;; (goto-char (point-min))
                   ;; (org-next-link)
                   ;; (org-open-at-point)
                   ))))))
  (setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (org-link-set-parameters
   "tsfile"
   :follow (lambda (path) (my-handle-tsfile-link path))
   :help-echo "Opens the linked file with your default application")
  (add-to-list 'org-modules 'org-habit)
  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED"
                 ("ARCHIVE" . t))
                ("WAITING"
                 ("WAITING" . t))
                (done
                 ("WAITING"))
                ("TODO"
                 ("WAITING")
                 ("CANCELLED"))
                ("NEXT"
                 ("WAITING"))
                ("STARTED"
                 ("WAITING"))
                ("DONE"
                 ("WAITING")
                 ("CANCELLED")))))
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

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
  (setq org-todo-keywords (quote
                           ((sequence "TODO(t)" "NEXT(n!)" "STARTED(s!)" "WAITING(w@)" "SOMEDAY(S@/!)" "|" "DONE(d!/!)" "CANCELLED(c@)"))))
  (setq org-todo-repeat-to-state "NEXT")

  ;; https://github.com/novoid/dot-emacs/blob/28c146f785c1d87dc821514e8448e3dfe82e56ce/config.org
  (setq org-log-done (quote time))
  (setq org-id-method 'org)
  (setq org-log-into-drawer t)
  (setq org-log-redeadline (quote note));; record when the deadline date of a tasks is modified
  (setq org-log-reschedule (quote time))
  (setq org-return-follows-link t)
  (setq org-remove-highlights-with-change nil)
  (setq org-read-date-prefer-future 'time)
  (setq org-deadline-warning-days 1)
  (setq org-enforce-todo-dependencies t)
  (setq org-startup-indented t)
  (setq org-adapt-indentation nil);; do not indent drawers/body according to heading level
  (setq org-insert-heading-respect-content nil)
  (setq org-reverse-note-order nil)
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)
  (setq org-hierarchical-todo-statistics t)
  (setq org-log-note-clock-out t)
  (setq org-yank-adjusted-subtrees t)
  (setq org-list-demote-modify-bullet (quote (("+" . "-")
                                              ("*" . "-")
                                              ("1." . "-")
                                              ("1)" . "-"))))
  (setq org-blank-before-new-entry (quote ((heading . t)
                                           (plain-list-item . nil))))
  (font-lock-add-keywords
   'org-mode '(("\\(@@html:<kbd>@@\\) \\(.*\\) \\(@@html:</kbd>@@\\)"
                (1 font-lock-comment-face prepend)
                (2 font-lock-function-name-face)
                (3 font-lock-comment-face prepend))))
  ;; Open links and files with RET in normal state
  (evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)

  (evil-define-key 'normal org-mode-map
    "t" 'org-todo
    ",'" 'org-edit-special
    ",c" 'org-capture
    ",Cc" 'org-clock-cancel
    ",Ci" 'org-clock-in
    ",Co" 'org-clock-out
    ",Cr" 'org-resolve-clocks
    ",dd" 'org-deadline
    ",ds" 'org-schedule
    ",dt" 'org-time-stamp
    ",dT" 'org-time-stamp-inactive
    ",ee" 'org-export-dispatch

    ",a" 'org-agenda

    ",Te" 'org-toggle-pretty-entities
    ",Ti" 'org-toggle-inline-images
    ",Tl" 'org-toggle-link-display
    ",Tt" 'org-show-todo-tree
    ",TT" 'org-todo
    ",TV" 'space-doc-mode
    ",Tx" 'org-toggle-latex-fragment

    ;; More cycling options (timestamps, headlines, items, properties)
    ",L" 'org-shiftright
    ",H" 'org-shiftleft
    ",J" 'org-shiftdown
    ",K" 'org-shiftup

    ;; Change between TODO sets
    "C-S-l" 'org-shiftcontrolright
    "C-S-h" 'org-shiftcontrolleft
    "C-S-j" 'org-shiftcontroldown
    "C-S-k" 'org-shiftcontrolup

    ;; Subtree editing
    ",sa" 'org-archive-subtree
    ",sb" 'org-tree-to-indirect-buffer
    ",sh" 'org-promote-subtree
    ",sj" 'org-move-subtree-down
    ",sk" 'org-move-subtree-up
    ",sl" 'org-demote-subtree
    ",sn" 'org-narrow-to-subtree
    ",sN" 'widen
    ",sr" 'org-refile
    ",ss" 'org-sparse-tree
    ",sS" 'org-sort

    ;; tables
    ",ta" 'org-table-align
    ",tb" 'org-table-blank-field
    ",tc" 'org-table-convert
    ",tdc" 'org-table-delete-column
    ",tdr" 'org-table-kill-row
    ",te" 'org-table-eval-formula
    ",tE" 'org-table-export
    ",th" 'org-table-previous-field
    ",tH" 'org-table-move-column-left
    ",tic" 'org-table-insert-column
    ",tih" 'org-table-insert-hline
    ",tiH" 'org-table-hline-and-move
    ",tir" 'org-table-insert-row
    ",tI" 'org-table-import
    ",tj" 'org-table-next-row
    ",tJ" 'org-table-move-row-down
    ",tK" 'org-table-move-row-up
    ",tl" 'org-table-next-field
    ",tL" 'org-table-move-column-right
    ",tn" 'org-table-create
    ",tN" 'org-table-create-with-table.el
    ",tr" 'org-table-recalculate
    ",ts" 'org-table-sort-lines
    ",ttf" 'org-table-toggle-formula-debugger
    ",tto" 'org-table-toggle-coordinate-overlays
    ",tw" 'org-table-wrap-region

    ;; Multi-purpose keys
    ",*" 'org-ctrl-c-star
    ",-" 'org-ctrl-c-minus
    ",#" 'org-update-statistics-cookies
    ",RET"   'org-ctrl-c-ret
    ",M-RET" 'org-meta-return
    ;; attachments
    ",A" 'org-attach
    ;; insertion
    ",ii" 'org-insert-item
    ",id" 'org-insert-drawer
    ",ie" 'org-set-effort
    ",if" 'org-footnote-new
    ",ih" 'org-insert-heading
    ",iH" 'org-insert-heading-after-current
    ",iK" 'spacemacs/insert-keybinding-org
    ",il" 'org-insert-link
    ",ip" 'org-set-property
    ",is" 'org-insert-subheading
    ",it" 'org-set-tags
    ",xb" (spacemacs|org-emphasize spacemacs/org-bold ?*)
    ",xc" (spacemacs|org-emphasize spacemacs/org-code ?~)
    ",xi" (spacemacs|org-emphasize spacemacs/org-italic ?/)
    ",xo" 'org-open-at-point
    ",xr" (spacemacs|org-emphasize spacemacs/org-clear ? )
    ",xs" (spacemacs|org-emphasize spacemacs/org-strike-through ?+)
    ",xu" (spacemacs|org-emphasize spacemacs/org-underline ?_)
    ",xv" (spacemacs|org-emphasize spacemacs/org-verbatim ?=))
  ;; region manipulation
  (evil-define-key 'visual org-mode-map
    ",il" 'org-insert-link
    ",id" 'org-insert-drawer
    ",xb" (spacemacs|org-emphasize spacemacs/org-bold ?*)
    ",xc" (spacemacs|org-emphasize spacemacs/org-code ?~)
    ",xi" (spacemacs|org-emphasize spacemacs/org-italic ?/)
    ",xo" 'org-open-at-point
    ",xr" (spacemacs|org-emphasize spacemacs/org-clear ? )
    ",xs" (spacemacs|org-emphasize spacemacs/org-strike-through ?+)
    ",xu" (spacemacs|org-emphasize spacemacs/org-underline ?_)
    ",xv" (spacemacs|org-emphasize spacemacs/org-verbatim ?=))

  ;; Evilify the calendar tool on C-c .
  (define-key org-read-date-minibuffer-local-map (kbd "M-h")
    (lambda () (interactive)
      (org-eval-in-calendar '(calendar-backward-day 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-l")
    (lambda () (interactive)
      (org-eval-in-calendar '(calendar-forward-day 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-k")
    (lambda () (interactive)
      (org-eval-in-calendar '(calendar-backward-week 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-j")
    (lambda () (interactive)
      (org-eval-in-calendar '(calendar-forward-week 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-H")
    (lambda () (interactive)
      (org-eval-in-calendar '(calendar-backward-month 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-L")
    (lambda () (interactive)
      (org-eval-in-calendar '(calendar-forward-month 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-K")
    (lambda () (interactive)
      (org-eval-in-calendar '(calendar-backward-year 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-J")
    (lambda () (interactive)
      (org-eval-in-calendar '(calendar-forward-year 1)))))

(use-package htmlize
  :straight t
  :commands (org-html-export-as-html org-html-export-as-html))

(use-package org-clock
  :after org
  :commands (org-clock-persistence-insinuate)
  :init (org-clock-persistence-insinuate)
  :config
  (setq org-clock-persist 'history)
  (setq org-clock-idle-time 10))

;; (use-package org-expiry
;;   :after org
;;   :commands (org-expiry-insinuate
;;              org-expiry-deinsinuate
;;              org-expiry-insert-created
;;              org-expiry-insert-expiry
;;              org-expiry-add-keyword
;;              org-expiry-archive-subtree
;;              org-expiry-process-entry
;;              org-expiry-process-entries)
;;   :init (org-expiry-insinuate))

(with-eval-after-load 'org-indent
  (diminish 'org-indent-mode))

(use-package org-capture
  :config
  (setq org-capture--clipboards t)
  (evil-define-key 'normal 'org-capture-mode
    ",a" 'org-capture-kill
    ",c" 'org-capture-finalize
    ",k" 'org-capture-kill
    ",r" 'org-capture-refile)

  (setq org-capture-templates
        '(("s" "Some day" entry
           (file+olp "~/Dropbox/ORG/notes.org" "notes" "some day")
           "*** TODO %? %^C %^G\n%U")
          ("n" "notes" entry
           (file+olp "~/Dropbox/ORG/notes.org" "notes" "note")
           "*** %?\n   %U")
          ("c" "code snipptes" entry
           (file+olp "~/Dropbox/ORG/snipptes.org" "snipptes")
           "**** %?\n%U")
          ("f" "file TODOs" entry
           (file "~/Dropbox/ORG/gtd.org")
           "* TODO %? \n %a\n%U")
          ("t" "TODOs" entry
           (file+olp "~/Dropbox/ORG/gtd.org" "misc")
           "* TODO %? \n%U"))))

(use-package org-web-tools
  :straight t
  :config
  (defun mkm-org-capture/link ()
    "Make a TODO entry with a link in clipboard. Page title is used as task heading."
    (let* ((url-string (s-trim (x-get-clipboard)))
           (pdf (string-suffix-p "pdf" url-string)))
      (unless pdf
        (let ((page-title (org-web-tools--html-title (org-web-tools--get-url url-string))))
          (concat "* TODO "
                  page-title " %^g"
                  "\n\t:PROPERTIES:\n\t:URL: "
                  url-string
                  "\n\t:END:\n\s\s%?"
                  )))))
  (add-to-list
   'org-capture-templates
   '("l" "Capture a link from clipboard" entry
     (file+olp "~/Dropbox/ORG/notes.org" "notes" "read")
     #'mkm-org-capture/link)))

(use-package evil-org
  :straight t
  :defer t
  :diminish evil-org-mode
  :init
  (setq evil-org-key-theme `(textobjects
                             navigation
                             additional)))

(use-package org-agenda
  :defer t
  :init
  (setq org-agenda-restore-windows-after-quit t)
  (dolist (prefix '(("mC" . "clocks")
                    ("md" . "dates")
                    ("mi" . "insert")
                    ("ms" . "trees/subtrees")))
    (evil-define-key 'normal 'org-agenda-mode
      (car prefix) (cdr prefix)))
  (evil-define-key 'normal 'org-agenda-mode
    ",a" 'org-agenda
    ",Cc" 'org-agenda-clock-cancel
    ",Ci" 'org-agenda-clock-in
    ",Co" 'org-agenda-clock-out
    ",dd" 'org-agenda-deadline
    ",ds" 'org-agenda-schedule
    ",ie" 'org-agenda-set-effort
    ",ip" 'org-agenda-set-property
    ",it" 'org-agenda-set-tags
    ",sr" 'org-agenda-refile)
  :config
  (evilified-state-evilify-map org-agenda-mode-map
    :mode org-agenda-mode
    :bindings
    "j" 'org-agenda-next-line
    "k" 'org-agenda-previous-line
    (kbd "M-j") 'org-agenda-next-item
    (kbd "M-k") 'org-agenda-previous-item
    (kbd "M-h") 'org-agenda-earlier
    (kbd "M-l") 'org-agenda-later
    (kbd "gd") 'org-agenda-toggle-time-grid
    (kbd "gr") 'org-agenda-redo
    (kbd "M-RET") 'org-agenda-show-and-scroll-up))

(use-package org-bullets
  :straight t
  :defer t
  :init (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package org-projectile
  :straight t
  :commands (org-projectile-capture-for-current-project)
  :init
  (setq org-projectile-projects-file "~/Dropbox/ORG/project.org")
  (setq org-projectile-capture-template "* TODO %? %^G\n%U")
  (spacemacs/set-leader-keys "pc" 'org-projectile-capture-for-current-project)
  (with-eval-after-load 'org-capture
    (unless (featurep 'org-projectile)
      (require 'org-projectile))))

(use-package ob
  :defer t
  :init
  (defun spacemacs//org-babel-do-load-languages ()
    "Load all the languages declared in `org-babel-load-languages'."
    (org-babel-do-load-languages 'org-babel-load-languages
                                 org-babel-load-languages))
  (add-hook 'org-mode-hook 'spacemacs//org-babel-do-load-languages)
  ;; Fix redisplay of inline images after a code block evaluation.
  (defun spacemacs/ob-fix-inline-images ()
    "Fix redisplay of inline images after a code block evaluation."
    (when org-inline-image-overlays
      (org-redisplay-inline-images)))
  (add-hook 'org-babel-after-execute-hook 'spacemacs/ob-fix-inline-images))

(use-package org-fancy-priorities
  :straight t
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("HIGH" "MID" "OPTIONAL" "LOW")))

(yq/get-modules "org-agenda.el")

(use-package org-mru-clock
  :straight t
  :init
  (setq org-mru-clock-completing-read #'ivy-completing-read)
  (setq org-mru-clock-keep-formatting t)
  (spacemacs/set-leader-keys "ci" 'org-mru-clock-in)
  (spacemacs/set-leader-keys "co" 'org-clock-out)
  (spacemacs/set-leader-keys "cd" 'org-clock-display)
  (spacemacs/set-leader-keys "cj" 'org-mru-clock-select-recent-task)
  (setq org-mru-clock-how-many 100
        org-mru-clock-completing-read #'ivy-completing-read))

(use-package org-sticky-header
  :straight ( :host github :repo "alphapapa/org-sticky-header")
  :after org-mode
  :hook (org-mode .org-sticky-header-mode))