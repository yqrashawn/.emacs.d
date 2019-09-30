;;; org2.el ---  org packages -*- lexical-binding: t; -*-

;; global keybindings
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

(straight-use-package 'org)

(use-package org-starter
  :straight t
  :config
  (org-starter-define-directory "~/Dropbox/ORG/"
    :files
    '(("inbox.org"
       :key "i"
       :agenda t
       :required t
       :refile (:maxlevel . 1)
       :capture (("c"
                  "Inbox Entry"
                  entry
                  (file org-default-inbox-file)
                  "* %? %^G\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%i")))
      ("someday.org" :key "S" :agenda t :required t :refile (:maxlevel . 1))
      ("tasks.org" :key "t" :agenda t :required t :refile (:maxlevel . 1))
      ("media.org" :key "m" :required t :refile (:maxlevel . 1))
      ("diary.org" :key "A" :required t :refile (:maxlevel . 1))))
  (org-starter-def-capture "c" "Inbox" entry (file "inbox.org")
                           "* TODO %? %^G\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%i"
                           :jump-to-captured t)
  (spacemacs/set-leader-keys "2" 'org-starter-find-file-by-key))

(with-eval-after-load 'org
  ;; auto save all org buffers after various org operation
  (defun +org/save-all-buffers (&rest _) (interactive) (org-save-all-org-buffers))
  (add-hook 'org-capture-mode-hook #'evil-insert-state)
  (advice-add 'org-todo :after '+org/save-all-buffers)
  (advice-add 'org-store-log-note :after '+org/save-all-buffers)
  (advice-add 'org-refile :after '+org/save-all-buffers)
  (advice-add 'org-agenda-quit :after '+org/save-all-buffers)
  (advice-add 'org-agenda-priority :after '+org/save-all-buffers)
  (advice-add 'org-agenda-todo :after '+org/save-all-buffers)
  (advice-add 'org-agenda-refile :after '+org/save-all-buffers)
  (add-hook 'org-capture-after-finalize-hook #'+org/save-all-buffers)
  (advice-add 'org-archive-default-command :after '+org/save-all-buffers)

  (defmacro spacemacs|org-emphasize (fname char)
    "Make function for setting the emphasis in org mode"
    `(defun ,fname () (interactive)
            (org-emphasize ,char)))
  (defun +org-open-at-point-and-remove-output ()
    (interactive)
    (org-open-at-point)
    (org-babel-remove-result))

  (evil-define-key 'insert org-mode-map (kbd "<tab>") 'org-cycle)
  (evil-define-key 'normal org-mode-map
    (kbd "<tab>") 'org-cycle
    "t" 'org-todo
    ",'" 'org-edit-special
    ",," '+org-open-at-point-and-remove-output
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
  (+org-refile inbox '(("~/Dropbox/ORG/inbox.org" :level . 1)))

  (defun +org-read-datetree-date (d)
    "Parse a time string D and return a date to pass to the datetree functions."
    (let ((dtmp (nthcdr 3 (parse-time-string d))))
      (list (cadr dtmp) (car dtmp) (caddr dtmp))))

  (defun +org-refile-to-archive-datetree (&optional bfn)
    "Refile an entry to a datetree under an archive."
    (interactive)
    (require 'org-datetree)
    (let* ((bfn (or bfn (find-file-noselect (expand-file-name "~/Dropbox/ORG/diary.org"))))
           (datetree-date (+org-read-datetree-date (org-read-date t nil))))
      (org-refile nil nil (list nil (buffer-file-name bfn) nil
                                (with-current-buffer bfn
                                  (save-excursion
                                    (org-datetree-find-date-create datetree-date)
                                    (point))))))
    (setq this-command '+org-refile-to-journal))

  (defhydra +org-workflow-hydra (:color blue :hint nil)
    "
  visit            refile          actions
 ------------------------------------------
  _i_nbox.org     _M_edia.org    _A_rchive
  _t_ask.org      _T_ask.org
  _p_rojects.org  _P_rojects.org
  _s_omday.org    _S_omday.org
                  _I_nbox.org
 "
    ("i" (lambda () (interactive) (find-file "~/Dropbox/ORG/inbox.org")))
    ("t" (lambda () (interactive) (find-file "~/Dropbox/ORG/tasks.org")))
    ("p" (lambda () (interactive) (find-file "~/Dropbox/ORG/projects.org")))
    ("s" (lambda () (interactive) (find-file "~/Dropbox/ORG/someday.org")))
    ("m" (lambda () (interactive) (find-file "~/Dropbox/ORG/media.org")))
    ("T" +org-refile-task)
    ("P" +org-refile-project)
    ("S" +org-refile-someday)
    ("M" +org-refile-media)
    ("I" +org-refile-inbox)
    ("A" +org-refile-to-archive-datetree)
    ("j" org-next-visible-heading :exit nil)
    ("k" org-previous-visible-heading :exit nil)
    ("TAB" org-cycle :exit nil)
    ("<tab>" org-cycle :exit nil)
    ("." nil :exit t))
  (evil-define-key 'normal org-mode-map "." #'+org-workflow-hydra/body)

  ;; automatically update todo states depends on checkboxe states
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)  ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo))

(evil-define-key 'normal org-mode-map "." #'+org-workflow-hydra/body)

(with-eval-after-load 'org-capture
  (setq org-capture-templates '()))

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

;; obs
(use-package ob
  :defer t
  :init
  (setq org-babel-load-languages
        '((emacs-lisp . t)
          (clojure . t)
          (clojurescript . t)
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

;; https://github.com/krisajenkins/ob-mongo/tree/371bf19c7c10eab2f86424f8db8ab685997eb5aa
(use-package ob-mongo
  :straight t
  :after ob)

(use-package ob-clojurescript
  :straight t
  :after ob)

(use-package ob-async
  :straight t
  :after ob)

(use-package ob-restclient
  :straight t
  :after ob)

(use-package evil-org
  ;; key themes
  ;; org (org-agenda-open-link "[[file:~/.emacs.d/straight/repos/evil-org-mode/doc/keythemes.org::*Basic][Basic]]")
  ;; agenda (org-agenda-open-link "[[file:~/.emacs.d/straight/repos/evil-org-mode/README.org::*Agenda][Agenda]]")
  :straight t
  :diminish evil-org-mode
  :after (evil org)
  :hook
  (org-mode . evil-org-mode)
  :custom
  (evil-org-key-theme '(textobjects insert navigation additional shift todo heading))
  :init
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (define-key evil-org-mode-map ">" 'evil-org->)
  (define-key evil-org-mode-map "<" 'evil-org-<))

(use-package org-jira
  :straight t
  :after (org org-clock)
  :custom
  (jiralib-url "https://conflux-bounty.atlassian.net/")
  (org-jira-progress-issue-flow
   '(("Backlog" . "In Progress")
     ("In Progress" . "Done"))))

(use-package ejira
  :straight (:host github :repo "nyyManni/ejira"
                   :files
                   (:defaults (:exclude "helm-ejira.el") "ejira*"))
  :commands (ejira-update-my-projects)
  :custom
  (jiralib2-url "https://conflux-bounty.atlassian.net")
  (jiralib2-auth 'token)
  (ejira-org-directory "~/jira")
  (ejira-priorities-alist    '(("Highest" . ?A)
                               ("High"    . ?B)
                               ("Medium"  . ?C)
                               ("Low"     . ?D)
                               ("Lowest"  . ?E)))
  (ejira-todo-states-alist   '(("To Do"       . 1)
                               ("In Progress" . 2)
                               ("Done"        . 3)))
  (ejira-projects '("CBV9" "DAG"))
  :init
  ;; (load-library "~/.emacs.d/.authinfo.el.gpg")
  ;; (setq jiralib2-user-login-name my-jira-login-email
  ;;       jiralib2-token my-jira-token)
  :config
  (add-hook 'jiralib2-post-login-hook #'ejira-guess-epic-sprint-fields)
  (require 'ejira-agenda)
  (add-to-list 'org-agenda-files ejira-org-directory)
  (org-add-agenda-custom-command
   '("j" "My JIRA issues"
     ((ejira-jql "resolution = unresolved and assignee = currentUser()"
                 ((org-agenda-overriding-header "Assigned to me")))))))
