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
  :straight org-plus-contrib
  :init
  (setq org-insert-mode-line-in-empty-file t

        ;; automatically change status of a heading to DONE when all children are done
        org-enforce-todo-checkbox-dependencies t
        org-agenda-inhibit-startup nil
        org-cycle-emulate-tab 'white

        org-catch-invisible-edits 'smart
        org-goto-auto-isearch nil
        org-goto-interface 'outline-path-comletion

        ;; src block have same indentation with #+BEGIN_SRC
        org-edit-src-content-indentation 0
        org-M-RET-may-split-line nil
        org-archive-location "~/Dropbox/ORG/Archive/%s_archive::"

        ;; make refile able to use top level heading as target
        org-refile-use-outline-path 'file

        ;; make refile prompt fancy, separate heading with /
        org-outline-path-complete-in-steps nil
        org-refile-use-cache t
        org-refile-targets '(("~/Dropbox/ORG/gtd.org" :maxlevel . 2)
                             ("~/Dropbox/ORG/project.org" :maxlevel . 1)
                             ("~/Dropbox/ORG/notes.org" :maxlevel . 2)
                             ("~/Dropbox/ORG/snippets.org" :maxlevel . 2))
        org-tag-alist '(("OFFICE" . ?w) ("HOME" . ?h))
        org-startup-with-inline-images t
        org-src-fontify-natively t
        org-imenu-depth 8
        org-src-tab-acts-natively t
        org-clock-persist-file (concat spacemacs-cache-directory "org-clock-save.el")
        org-id-locations-file (concat spacemacs-cache-directory ".org-id-locations")
        org-publish-timestamp-directory (concat spacemacs-cache-directory ".org-timestamps/")
        org-directory "~/Dropbox/ORG" ;; needs to be defined for `org-default-notes-file'
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-log-done 'time
        org-image-actual-width nil

        ;; agenda
        org-agenda-include-diary t
        org-stuck-projects
        '("+LEVEL=1/-DONE"
          ("TODO" "NEXT" "SOMEDAY" "PRIORITY=\"C\"")
          ("NOSTUCK")
          ""))

  ;; recache refile targets if emacs idle for 8min
  (run-with-idle-timer 400 t (lambda ()
                               (org-refile-cache-clear)
                               (org-refile-get-targets)))

  (require 'org-agenda)
  (setq org-log-note-headings '((done . "CLOSING NOTE T: %t")
                                (state . "State %-12s from %-12S T: %t")
                                (note . "Note taken on T: %t")
                                (reschedule . "Rescheduled from %S on T: %t")
                                (delschedule . "Not scheduled, was %S on T: %t")
                                (redeadline . "New deadline from %S on T: %t")
                                (deldeadline . "Removed deadline, was %S on T: %t")
                                (refile . "Refiled on T: %t")
                                (clock-out . "Clocked out on T: %t")))

  ;; recent activity
  ;; https://stackoverflow.com/questions/8039416/custom-searches-using-timestamps-in-logbook-in-org-mode
  (defun +org/find-state (&optional end)
    "Used to search through the logbook of subtrees.

    Looking for T:[2018-09-14 Fri 10:50] kind of time stamp in logbook."
    (let* ((closed (re-search-forward "^CLOSED: \\[" end t))
           (created (if (not closed) (re-search-forward "^:CREATED: \\[" end t)))
           (logbook (if (not closed) (re-search-forward ".*T: \\[" end t)))
           (result (or closed logbook created)))
      result))

  (defun +org/date-diff (start end &optional compare)
    "Calculate difference between  selected timestamp to current date.

  The difference between the dates is calculated in days.
  START and END define the region within which the timestamp is found.
  Optional argument COMPARE allows for comparison to a specific date rather than to current date."
    (let* ((start-date (if compare compare (calendar-current-date))))
      (- (calendar-absolute-from-gregorian start-date) (org-time-string-to-absolute (buffer-substring-no-properties start end)))))

  (defun +org/last-update-before (since)
    "List Agenda items that updated before SINCE day."
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      ;; If DONE is non-nil, look for done keywords, if nil look for not-done
      (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
             (subtree-valid (save-excursion
                              (forward-line 1)
                              (if (and (< (point) subtree-end)
                                       ;; Find the timestamp to test
                                       (+org/find-state subtree-end))
                                  (let ((startpoint (point)))
                                    (forward-word 3)
                                    ;; Convert timestamp into days difference from today
                                    (+org/date-diff startpoint (point)))
                                (point-max)))))
        (if (and subtree-valid (>= subtree-valid since))
            next-headline
          nil))))

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
  (add-hook 'org-mode-hook (lambda () (hs-minor-mode 1)))
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (define-key global-map "\C-cc" 'org-capture)
  (define-key global-map "\C-cb" 'org-switchb)
  (defun yq/org-blog-insert-id-after-heading ()
    (if (y-or-n-p "Create a unique ID for this section?")
        (org-id-get-create)))
  :config
  (defun +org/save-all-buffers (&optional arg) (interactive) (org-save-all-org-buffers))
  (add-hook 'org-capture-mode-hook #'evil-insert-state)
  (advice-add 'org-todo :after '+org/save-all-buffers)
  (advice-add 'org-store-log-note :after '+org/save-all-buffers)
  (advice-add 'org-refile :after '+org/save-all-buffers)
  (advice-add 'org-agenda-quit :before '+org/save-all-buffers)
  (advice-add 'org-agenda-priority :before '+org/save-all-buffers)
  (advice-add 'org-agenda-todo :after '+org/save-all-buffers)
  (advice-add 'org-agenda-deadline :after '+org/save-all-buffers)
  (advice-add 'org-agenda-schedule :after '+org/save-all-buffers)
  (advice-add 'org-agenda-refile :after '+org/save-all-buffers)
  (add-hook 'org-capture-after-finalize-hook #'+org/save-all-buffers)
  (require 'org-id)
  ;; https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=887332
  ;; auto save all org buffers after archive
  (advice-add 'org-archive-default-command :after '+org/save-all-buffers)
  (evil-define-key 'normal org-mode-map (kbd "<tab>") 'org-cycle)

  (defun my-handle-tsfile-link (querystring)
    (let ((querystring
           (if (s-contains-p "/" querystring)
               (f-filename querystring)
             (s-replace " " ".*" querystring))))
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
                                                      (completing-read "Choose: " queryresults)))))))))
  (org-link-set-parameters
   "tsfile"
   :follow (lambda (path) (my-handle-tsfile-link path))
   :help-echo "Opens the linked file with your default application")

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
    (let (org-log-done org-log-states)  ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "NEXT(n!)"
           "STARTED(s!)"
           "WAITING(w@)"
           "|"
           "SOMEDAY(S@/!)"
           "DONE(d!/!)"
           "CANCELLED(c@)")))
  (setq org-todo-repeat-to-state "NEXT")

  ;; https://github.com/novoid/dot-emacs/blob/28c146f785c1d87dc821514e8448e3dfe82e56ce/config.org
  (setq org-id-method 'org)
  (setq org-id-include-domain t)
  (setq org-log-into-drawer t)
  (setq org-log-redeadline 'note) ;; record when the deadline date of a tasks is modified
  (setq org-log-reschedule 'time)
  (setq org-return-follows-link t)
  (setq org-remove-highlights-with-change nil)
  (setq org-read-date-prefer-future 'time)
  (setq org-deadline-warning-days 3)
  (setq org-enforce-todo-dependencies t)
  (setq org-startup-indented t)
  (setq org-adapt-indentation nil) ;; do not indent drawers/body according to heading level
  (setq org-insert-heading-respect-content t)
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)
  (setq org-hierarchical-todo-statistics t)
  (setq org-log-note-clock-out t)
  (setq org-yank-adjusted-subtrees t)
  (setq org-list-demote-modify-bullet '(("+" . "-")
                                        ("*" . "-")
                                        ("1." . "-")
                                        ("1)" . "-")))
  (setq org-blank-before-new-entry nil)
  (font-lock-add-keywords
   'org-mode '(("\\(@@html:<kbd>@@\\) \\(.*\\) \\(@@html:</kbd>@@\\)"
                (1 font-lock-comment-face prepend)
                (2 font-lock-function-name-face)
                (3 font-lock-comment-face prepend))))

  ;; Open links and files with RET in normal state
  ;; (evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)
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
    ",hh" 'org-insert-heading-respect-content
    ",ht" 'org-insert-todo-heading-respect-content
    ",hs" 'org-insert-subheading
    ",ih" 'org-insert-heading
    ",iH" 'org-insert-heading-after-current
    ",iK" 'spacemacs/insert-keybinding-org
    ",il" 'org-insert-link
    ",ip" 'org-set-property
    ",is" 'org-insert-subheading
    ",it" 'org-set-tags-command
    ",xb" (spacemacs|org-emphasize spacemacs/org-bold ?*)
    ",xc" (spacemacs|org-emphasize spacemacs/org-code ?~)
    ",xi" (spacemacs|org-emphasize spacemacs/org-italic ?/)
    ",xo" 'org-open-at-point
    ",xr" (spacemacs|org-emphasize spacemacs/org-clear "? ")
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
    ",xr" (spacemacs|org-emphasize spacemacs/org-clear "? ")
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

(use-package org-tempo)

(use-package htmlize
  :straight t
  :commands (org-html-export-as-html org-html-export-as-html))

(use-package org-clock
  :after org
  :commands (org-clock-persistence-insinuate)
  :init (org-clock-persistence-insinuate)
  :config
  (defun bh/remove-empty-drawer-on-clock-out ()
    (interactive)
    (save-excursion
      (beginning-of-line 0)
      (org-remove-empty-drawer-at (point))))
  (add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)
  (setq  org-clock-idle-time 10
         org-clock-into-drawer "CLOCKING"
         org-clock-continuously nil
         org-clock-persist t
         org-clock-in-switch-to-state "STARTED"
         org-clock-in-resume nil
         org-clock-report-include-clocking-task t
         org-clock-out-remove-zero-time-clocks t
         ;; Too many clock entries clutter up a heading
         org-log-into-drawer t))

(with-eval-after-load 'org-indent
  (diminish 'org-indent-mode))

(use-package org-capture
  :commands (org-capture)
  :init
  (setq org-capture-templates
        '(("n" "notes" entry
           (file+olp "~/Dropbox/ORG/snippets.org" "notes" "inbox")
           "*** %?  %^G
:PROPERTIES:
:CREATED: %U
:END:")
          ("S" "snipptes" entry
           (file+olp "~/Dropbox/ORG/snipptes.org" "snipptes" "inbox")
           "*** %?  %^G
:PROPERTIES:
:CREATED: %U
:END:")
          ("j" "Queue job" entry
           (file+olp+datetree
            "~/Dropbox/ORG/gtd.org" "queue")
           "** TODO %?  %^G
SCHEDULED: %^T
:PROPERTIES:
:CREATED: %U
:END:"
           ;; :tree-type week
           :clock-resume t)

          ("s" "Queue job" entry
           (file+olp+datetree
            "~/Dropbox/ORG/gtd.org" "queue")
           "** SOMEDAY %?  %^G
:PROPERTIES:
:CREATED: %U
:END:"
           ;; :tree-type week
           :clock-resume t)))
  :config
  (setq org-capture--clipboards t)
  (evil-define-key 'normal 'org-capture-mode
    ",a" 'org-capture-kill
    ",c" 'org-capture-finalize
    ",k" 'org-capture-kill
    ",r" 'org-capture-refile))

(use-package org-web-tools
  :straight t
  :after org
  :init
  (defun mkm-org-capture/link ()
    "Make a TODO entry with a link in clipboard. Page title is used as task heading."
    (let* ((url-string (s-trim (x-get-clipboard)))
           (pdf (string-suffix-p "pdf" url-string)))
      (unless pdf
        (let ((page-title (org-web-tools--html-title (org-web-tools--get-url url-string))))
          (concat "* TODO "
                  page-title " %^G"
                  "\n\t:PROPERTIES:\n\t:URL: "
                  url-string
                  "\n\t:CREATED: %U"
                  "\n\t:END:\n\s\s%?")))))

  (with-eval-after-load 'org-capture
    (add-to-list
     'org-capture-templates
     '("l" "Capture a link from clipboard" entry
       (file+olp "~/Dropbox/ORG/notes.org" "Reads" "read")
       #'mkm-org-capture/link))))

(use-package evil-org
  :straight t
  :defer t
  :diminish evil-org-mode
  :hook (org-mode . evil-org-mode)
  :init
  ;; https://github.com/Somelauw/evil-org-mode/blob/b6d652a9163d3430a9e0933a554bdbee5244bbf6/doc/keythemes.org
  (setq evil-org-key-theme
        `(textobjects
          navigation
          additional))
  (evil-define-key 'normal evil-org-mode-map ">" 'evil-org->)
  (evil-define-key 'normal evil-org-mode-map "<" 'evil-org-<))

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
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package org-projectile
  :straight t
  :commands (org-projectile-capture-for-current-project)
  :init
  (org-projectile-single-file)
  (setq org-projectile-projects-file "~/Dropbox/ORG/project.org")
  (setq org-projectile-capture-template "* TODO %?  %^G
SCHEDULED: %^T
:PROPERTIES:
:CREATED: %U
:END:")
  (setq org-projectile-per-project-filepath nil)
  (spacemacs/set-leader-keys "pc" 'org-projectile-capture-for-current-project)
  (with-eval-after-load 'org-capture
    (unless (featurep 'org-projectile)
      (require 'org-projectile))))

(use-package org-download
  :straight t
  :after org
  :hook ((dired-mode org-mode) . org-download-enable))

(use-package ob
  :defer t
  :init
  (setq org-babel-load-languages
        '((emacs-lisp . t)
          (clojure . t)
          (shell . t)
          (restclient . t)
          (js . t)))
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

(use-package ob-async
  :straight t
  :after ob)

(use-package ob-restclient
  :straight t
  :after ob)

(use-package org-fancy-priorities
  :straight t
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("HIGH" "MID" "OPTIONAL" "LOW")))

(yq/get-modules "org-agenda.el")

(use-package org-mru-clock
  :straight t
  :init
  (defhydra hydra-org-clock (:color blue :hint nil)
    "
Clock   In/out^     ^Edit^   ^Summary     (_?_)
-----------------------------------------
        _i_n         _e_dit   _g_oto entry
        _c_ontinue   _C_ancel _d_isplay
        _o_ut        ^ ^      _r_eport
      "
    ("i" org-mru-clock-in)
    ("o" org-clock-out)
    ("c" org-clock-in-last)
    ("e" org-clock-modify-effort-estimate)
    ("C" org-clock-cancel)
    ("q" nil)
    ("g" org-clock-goto)
    ("d" org-clock-display)
    ("r" org-clock-report)
    ("?" (org-info "Clocking commands")))
  (setq org-mru-clock-completing-read #'ivy-completing-read)
  (setq org-mru-clock-keep-formatting t)
  (spacemacs/set-leader-keys "cc" 'hydra-org-clock/body)
  (spacemacs/set-leader-keys "ci" 'org-mru-clock-in)
  (spacemacs/set-leader-keys "co" 'org-clock-out)
  (spacemacs/set-leader-keys "cd" 'org-clock-display)
  (spacemacs/set-leader-keys "cD" 'org-clock-report)
  (spacemacs/set-leader-keys "cj" 'org-mru-clock-select-recent-task)
  (setq org-mru-clock-how-many 100
        org-mru-clock-completing-read #'ivy-completing-read))
;; (straight-use-package 'org-download)

(use-package org-sticky-header
  :straight ( :host github :repo "alphapapa/org-sticky-header")
  :after org-mode
  :hook (org-mode . org-sticky-header-mode))

;; (use-package ox-clip
;;   :straight t
;;   :after org
;;   :config
;;   (defun ox-clip-dwim ()
;;     "If the region is active, call ox-clip as normal. Otherwise, call ox-clip on whole buffer (or visible / narrowed section, if applicable)."
;;     (interactive)
;;     (if (region-active-p)
;;         (ox-clip-formatted-copy (region-beginning) (region-end))
;;       ;; if buffer is narrowed, this will work on visible; if not, it will capture whole buffer
;;       (ox-clip-formatted-copy (point-min) (point-max))))
;;   (define-key org-mode-map (kbd "C-c x") 'ox-clip-dwim))