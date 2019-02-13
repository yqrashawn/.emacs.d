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

;; https://github.com/raxod502/straight.el#installing-org-with-straightel
(require 'subr-x)
(straight-use-package 'git)

(defun org-git-version ()
  "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

(use-package org
  :straight org-plus-contrib
  :init
  (setq org-insert-mode-line-in-empty-file t

        ;; automatically change status of a heading to DONE when all children are done
        org-enforce-todo-checkbox-dependencies t
        org-agenda-inhibit-startup t
        org-cycle-emulate-tab 'white
        org-catch-invisible-edits 'smart
        org-goto-auto-isearch nil
        org-goto-interface 'outline-path-comletion

        ;; src block have same indentation with #+BEGIN_SRC
        org-edit-src-content-indentation 0
        org-M-RET-may-split-line nil
        org-archive-location "~/Dropbox/ORG/Archive/%s_archive::"

        ;; make refile able to use top level heading as target
        org-refile-use-outline-path t

        ;; make refile prompt fancy, separate heading with /
        org-outline-path-complete-in-steps nil
        org-refile-use-cache nil
        ;; org-tag-alist '(("OFFICE" . ?w) ("HOME" . ?h))
        org-startup-with-inline-images t
        org-src-fontify-natively t
        org-imenu-depth 8
        org-src-tab-acts-natively t
        org-clock-persist-file (concat spacemacs-cache-directory "org-clock-save.el")
        org-id-locations-file (concat spacemacs-cache-directory ".org-id-locations")
        org-publish-timestamp-directory (concat spacemacs-cache-directory ".org-timestamps/")
        org-directory (expand-file-name "~/Dropbox/ORG/") ;; needs to be defined for `org-default-notes-file'
        org-default-inbox-file (expand-file-name "inbox.org" org-directory)
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
  (setq org-refile-targets '((org-agenda-files :maxlevel . 1)))

  ;; (require 'org-agenda)
  ;; (setq org-log-note-headings '((done . "CLOSING NOTE T: %t")
  ;;                               (state . "State %-12s from %-12S T: %t")
  ;;                               (note . "Note taken on T: %t")
  ;;                               (reschedule . "Rescheduled from %S on T: %t")
  ;;                               (delschedule . "Not scheduled, was %S on T: %t")
  ;;                               (redeadline . "New deadline from %S on T: %t")
  ;;                               (deldeadline . "Removed deadline, was %S on T: %t")
  ;;                               (refile . "Refiled on T: %t")
  ;;                               (clock-out . "Clocked out on T: %t")))

  ;; Add global evil-leader mappings. Used to access org-agenda
  ;; functionalities – and a few others commands – from any other mode.

  ;; (spacemacs/set-leader-keys
  ;;   ;; org-agenda
  ;;   "ao#" 'org-agenda-list-stuck-projects
  ;;   "ao/" 'org-occur-in-agenda-files
  ;;   "aoa" 'org-agenda-list
  ;;   "aoc" 'org-capture
  ;;   "aoe" 'org-store-agenda-views
  ;;   "aoki" 'org-clock-in-last
  ;;   "aokj" 'org-clock-jump-to-current-clock
  ;;   "aoko" 'org-clock-out
  ;;   "aokr" 'org-resolve-clocks
  ;;   "aol" 'org-store-link
  ;;   "aom" 'org-tags-view
  ;;   "aoo" 'org-agenda
  ;;   "aos" 'org-search-view
  ;;   "aot" 'org-todo-list
  ;;   ;; SPC C- capture/colors
  ;;   "Cc" 'org-capture)

  ;; (org-update-statistics-cookies "ALL")
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (define-key global-map "\C-cc" 'org-capture)
  (define-key global-map "\C-cb" 'org-switchb)
  (defun yq/org-blog-insert-id-after-heading ()
    (if (y-or-n-p "Create a unique ID for this section?")
        (org-id-get-create)))
  :config
  (defmacro +org-refile (fn-suffix refile-targets)
    "Generate a command to call `org-refile' with modified targets."
    `(defun ,(intern (concat "+org-refile-" (symbol-name fn-suffix))) ()
       ,(format "`org-refile' to %S" refile-targets)
       (interactive)
       (org-refile-cache-clear)
       (let ((org-refile-target-verify-function nil)
             (org-refile-targets ,refile-targets))
         (call-interactively 'org-refile))))
  (+org-refile task '(("~/Dropbox/ORG/tasks.org" :level . 1)))
  (+org-refile projects '(("~/Dropbox/ORG/projects.org" :level . 2)))
  (+org-refile someday '(("~/Dropbox/ORG/someday.org" :level . 1)))
  (+org-refile media '(("~/Dropbox/ORG/media.org" :level . 1)))
  (defhydra +org-workflow-hydra (:color blue :hint nil)
    "
 visit            refile
------------------------------------------
 _i_nbox.org     _M_edia.org
 _t_ask.org      _T_ask.org
 _p_rojects.org  _P_rojects.org
 _s_omday.org    _S_omday.org
"
    ("i" (lambda () (interactive) (find-file "~/Dropbox/ORG/inbox.org")))
    ("t" (lambda () (interactive) (find-file "~/Dropbox/ORG/tasks.org")))
    ("p" (lambda () (interactive) (find-file "~/Dropbox/ORG/projects.org")))
    ("s" (lambda () (interactive) (find-file "~/Dropbox/ORG/someday.org")))
    ("m" (lambda () (interactive) (find-file "~/Dropbox/ORG/media.org")))
    ("T" +org-refile-task)
    ("P" +org-refile-project)
    ("S" +org-refile-someday)
    ("M" +org-refile-media))

  (define-key org-mode-map (kbd "C-c r") #'+org-workflow-hydra/body)

  (defun +org/save-all-buffers (&optional arg) (interactive) (org-save-all-org-buffers))
  (defun +org/save-all-buffers2 (&optional arg arg2) (interactive) (org-save-all-org-buffers))
  (add-hook 'org-capture-mode-hook #'evil-insert-state)
  (advice-add 'org-todo :after '+org/save-all-buffers)
  (advice-add 'org-store-log-note :after '+org/save-all-buffers)
  (advice-add 'org-refile :after '+org/save-all-buffers)
  (advice-add 'org-agenda-quit :after '+org/save-all-buffers)
  (advice-add 'org-agenda-priority :after '+org/save-all-buffers)
  (advice-add 'org-agenda-todo :after '+org/save-all-buffers)
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
  (setq org-id-include-domain nil)
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
        '(("c" "Inbox Entry" entry
           (file+olp org-default-inbox-file "Inbox")
           "* %? %^G\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%i")))

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
          (concat "* "
                  page-title " %^G"
                  "\n\t:PROPERTIES:\n\t:URL: "
                  url-string
                  "\n\t:CREATED: %U"
                  "\n\t:END:\n\s\s%?")))))

  (with-eval-after-load 'org-capture
    (add-to-list
     'org-capture-templates
     '("l" "Capture a link from clipboard" entry
       (file+olp org-default-inbox-file "Inbox")
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
    "j" 'org-agenda-next-item
    "k" 'org-agenda-previous-item
    (kbd "C-n") 'org-agenda-next-line
    (kbd "C-p") 'org-agenda-previous-line
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

(use-package counsel-org-clock
  :straight t
  :init
  ;; (spacemacs/set-leader-keys "4" 'counsel-org-clock-history)
  (spacemacs/set-leader-keys "4" 'counsel-org-clock-context))
(spacemacs/set-leader-keys (kbd "\`") (lambda () (interactive) (find-file "~/Dropbox/ORG/daily-review.org")))
(spacemacs/set-leader-keys (kbd "DEL") (lambda () (interactive) (find-file "~/Dropbox/ORG/weekly-review.org")))

(use-package org-mru-clock
  :straight t
  :init
  (add-hook 'org-clock-in-hook (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e" (concat "tell application \"org-clock-statusbar\" to clock in \"" (replace-regexp-in-string "\"" "\\\\\"" org-clock-current-task) "\""))))
  (add-hook 'org-clock-out-hook (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e" "tell application \"org-clock-statusbar\" to clock out")))
  (defhydra hydra-org-clock (:color blue :hint nil)
    "
Clock   In/out^     ^Edit^   ^Summary     (_?_)
-----------------------------------------
        _i_n         _e_dit   _g_oto entry
        _c_ontinue   _C_ancel _d_isplay
        _o_ut        _s_elect _r_eport
    "
    ("i" org-mru-clock-in)
    ("o" org-clock-out)
    ("s" org-mru-clock-select-recent-task)
    ("c" org-clock-in-last)
    ("e" org-clock-modify-effort-estimate)
    ("C" org-clock-cancel)
    ("q" nil)
    ("g" org-clock-goto)
    ("d" org-clock-display)
    ("r" org-clock-report)
    ("?" (org-info "Clocking commands")))
  (spacemacs/set-leader-keys "3" 'hydra-org-clock/body)
  (spacemacs/set-leader-keys "cc" 'hydra-org-clock/body)
  (spacemacs/set-leader-keys "ci" 'org-mru-clock-in)
  (spacemacs/set-leader-keys "co" 'org-clock-out)
  (spacemacs/set-leader-keys "cd" 'org-clock-display)
  (spacemacs/set-leader-keys "cD" 'org-clock-report)
  (spacemacs/set-leader-keys "cj" 'org-mru-clock-select-recent-task)
  (setq org-mru-clock-how-many 100
        org-mru-clock-completing-read #'ivy-completing-read
        org-mru-clock-keep-formatting t))

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

;; Hydra for org agenda (graciously taken from Spacemacs)
(defhydra hydra-org-agenda (:pre (setq which-key-inhibit t)
                                 :post (setq which-key-inhibit nil)
                                 :hint none)
  "
Org agenda (_q_uit)

^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _gt_ go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 _._ toggle hydra
^^              ^^                       ^^                 ^^
"
  ;; Entry
  ("j" org-agenda-next-item)
  ("k" org-agenda-previous-item)
  ("hA" org-agenda-archive-default)
  ("hk" org-agenda-kill)
  ("hp" org-agenda-priority)
  ("hr" org-agenda-refile)
  ("h:" org-agenda-set-tags)
  ("ht" org-agenda-todo)
  ;; Visit entry
  ("o"   link-hint-open-link :exit t)
  ("<tab>" org-agenda-goto :exit t)
  ("TAB" org-agenda-goto :exit t)
  ("SPC" org-agenda-show-and-scroll-up)
  ("RET" org-agenda-switch-to :exit t)
  ;; Date
  ("dt" org-agenda-date-prompt)
  ("dd" org-agenda-deadline)
  ("+" org-agenda-do-date-later)
  ("-" org-agenda-do-date-earlier)
  ("ds" org-agenda-schedule)
  ;; View
  ("vd" org-agenda-day-view)
  ("vw" org-agenda-week-view)
  ("vt" org-agenda-fortnight-view)
  ("vm" org-agenda-month-view)
  ("vy" org-agenda-year-view)
  ("vn" org-agenda-later)
  ("vp" org-agenda-earlier)
  ("vr" org-agenda-reset-view)
  ;; Toggle mode
  ("ta" org-agenda-archives-mode)
  ("tA" (org-agenda-archives-mode 'files))
  ("tr" org-agenda-clockreport-mode)
  ("tf" org-agenda-follow-mode)
  ("tl" org-agenda-log-mode)
  ("td" org-agenda-toggle-diary)
  ;; Filter
  ("fc" org-agenda-filter-by-category)
  ("fx" org-agenda-filter-by-regexp)
  ("ft" org-agenda-filter-by-tag)
  ("fr" org-agenda-filter-by-tag-refine)
  ("fh" org-agenda-filter-by-top-headline)
  ("fd" org-agenda-filter-remove-all)
  ;; Clock
  ("cq" org-agenda-clock-cancel)
  ("cj" org-agenda-clock-goto :exit t)
  ("ci" org-agenda-clock-in :exit t)
  ("co" org-agenda-clock-out)
  ;; Other
  ("q" nil :exit t)
  ("gd" org-agenda-goto-date)
  ("gt" org-agenda-goto-today)
  ;; ("." org-agenda-goto-today)
  ("." nil :exit t)
  ("gr" org-agenda-redo))

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd ".") 'hydra-org-agenda/body)
  (add-hook 'org-agenda-mode-hook 'hydra-org-agenda/body))
(spacemacs/set-leader-keys "2" (lambda () (interactive) (org-agenda nil "r")))

(use-package org-brain
  :straight t
  :disabled
  :after org
  :init
  (setq org-brain-path "~/Dropbox/ORG/BRAIN/")
  ;; For Evil users
  (evil-set-initial-state 'org-brain-visualize-mode 'emacs)
  (add-to-list 'org-structure-template-alist '("d" . "description"))
  :config
  (setq org-id-track-globally t)
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" )
        org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12))