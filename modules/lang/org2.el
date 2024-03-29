;;; org2.el ---  org packages -*- lexical-binding: t; -*-

(with-eval-after-load 'org
  (with-eval-after-load 'prettify-utils
    (prettify-utils-add-hook org-mode
                             ("[-]" "❍")
                             ("[X]" "☑")
                             ("[ ]" "☐")))

  (add-hook 'org-open-at-point-functions #'doom-set-jump-h)

  ;; global keybindings
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (define-key global-map "\C-cc" 'org-capture)

  ;; custom variable
  (setq org-log-done 'time)
  (setq org-link-context-for-files t)
  (setq org-link-file-path-type 'relative)
  (setq org-image-actual-width nil)
  (setq org-startup-indented t)
  (setq org-insert-heading-respect-content t)
  (setq org-display-inline-images t)
  (setq org-redisplay-inline-images t)
  (setq org-startup-with-inline-images "inlineimages")
  (setq org-indirect-buffer-display 'current-window)

  ;; enable org template
  (add-to-list 'org-modules 'org-tempo)

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

  ;; keybindings
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
    ;; ",ds" 'org-schedule
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
    (kbd "C-S-l") 'org-shiftcontrolright
    (kbd "C-S-h") 'org-shiftcontrolleft
    (kbd "C-S-j") 'org-shiftcontroldown
    (kbd "C-S-k") 'org-shiftcontrolup

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

  ;; refile
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

  ;; archive with datetree
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

  ;; hydra
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

  ;; automatically update todo list states depends on checkboxe states
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)  ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  ;; make org-store-link ask for description
  ;; https://emacs.stackexchange.com/questions/13093/get-org-link-to-insert-link-description-automatically/13104#13104
  ;; (defun +org-link-describe (link desc)
  ;;   (if (file-exists-p link)
  ;;       desc
  ;;     (read-string "Description: " desc)))
  ;; (setf org-make-link-description-function '+org-link-describe)

  ;; (defun +my-bable-to-buffer ()
  ;;   "A function to efficiently feed babel code block result to a separate buffer"
  ;;   (interactive)
  ;;   (org-open-at-point)
  ;;   (org-babel-remove-result))
  ;; (add-hook 'org-babel-after-execute-hook '+my-bable-to-buffer)

  ;; ob
  (setq org-babel-load-languages
        (append
         '((emacs-lisp . t)
           (clojure . t)
           (clojurescript . t)
           (shell . t)
           (restclient . t)
           (js . t))
         org-babel-load-languages)))

;; obs
(use-feature ob
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

(use-package org-download
  :straight t
  :after org
  :hook (dired-mode . org-download-enable)
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))

(use-package org-starter
  :straight t
  :config
  (org-starter-define-directory "~/Dropbox/ORG/"
    :add-to-path t
    :agenda t
    :files
    '(("snippets.org" :key "s" :agenda nil :required nil)))
  (setq org-starter-extra-find-file-map '(("j" org-journal-open-current-journal-file "Today")))
  ;; (org-starter-def-capture "c" "Inbox" entry (file "inbox.org")
  ;;                          "* TODO %? %^G\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%i"
  ;;                          :jump-to-captured t)
  ;; (spacemacs/set-leader-keys "2" 'org-starter-find-file-by-key)
  (spacemacs/set-leader-keys "2" nil)
  (spacemacs/set-leader-keys "2s" (defl (find-file "~/Dropbox/ORG/snippets.org")))
  (spacemacs/set-leader-keys "2j" #'org-journal-open-current-journal-file)
  )

(with-eval-after-load 'org-capture
  (setq org-capture-templates '())

  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                 "\n")))

  (add-to-list 'org-capture-templates
               '("h"                    ;`org-capture' binding + h
                 "Hugo"))

  (add-to-list 'org-capture-templates
               '("hp"                ;`org-capture' binding + h
                 "Hugo post"
                 entry
                 ;; It is assumed that below file is present in `org-directory'
                 ;; and that it has a "Blog Ideas" heading. It can even be a
                 ;; symlink pointing to the actual location of all-posts.org!
                 (file+olp "~/workspace/home/yqrashawn.github.io/content-org/posts.org" "Inbox")
                 (function org-hugo-new-subtree-post-capture-template))))

(use-package org-web-tools
  :straight t
  :commands (org-web-tools-insert-link-for-url)
  :after org
  :config
  (evil-define-key 'normal org-mode-map ",iL" #'org-web-tools-insert-link-for-url))

;; ox
(use-package ox-hugo
  :straight t
  :after org
  :custom
  (org-hugo-use-code-for-kbd t)
  (org-hugo-auto-set-lastmod t))

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
  :commands (evil-org-set-key-theme)
  :hook
  (org-mode . evil-org-mode)
  (evil-org-mode . evil-org-set-key-theme)
  :custom
  (evil-org-key-theme '(textobjects insert navigation additional shift todo heading))
  :init
  (add-hook 'evil-org-mode-hook #'evil-org-set-key-theme)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-define-key 'normal org-mode-map
    ">" #'evil-org->
    "<" #'evil-org-<))

;; add :asc to wrap js block in async func
(with-eval-after-load 'ob-js
  ;; set NODE_PATH for ob-js
  (let ((old-node-path (getenv "NODE_PATH")))
    (setenv "NODE_PATH"
            (if old-node-path
                (concat "~/local/bin/node_modules" ":" old-node-path)
              "~/local/bin/node_modules")))
  (defun org-babel-execute:js (body params)
    "Execute a block of Javascript code with org-babel.
This function is called by `org-babel-execute-src-block'."
    (let* ((org-babel-js-cmd (or (cdr (assq :cmd params)) org-babel-js-cmd))
           (session (cdr (assq :session params)))
           (result-type (cdr (assq :result-type params)))
           (full-body (org-babel-expand-body:generic
                       body params (org-babel-variable-assignments:js params)))
           (full-body (if (seq-contains params '(:asc))
                          (format "return (async function() {\n%s\n})()" full-body)
                        full-body))
           (result (cond
                    ;; no session specified, external evaluation
                    ((string= session "none")
                     (let ((script-file (org-babel-temp-file "js-script-")))
                       (with-temp-file script-file
                         (insert
                          ;; return the value or the output
                          (if (string= result-type "value")
                              (format org-babel-js-function-wrapper full-body)
                            full-body)))
                       (org-babel-eval
                        (format "%s %s" org-babel-js-cmd
                                (org-babel-process-file-name script-file)) "")))
                    ;; Indium Node REPL.  Separate case because Indium
                    ;; REPL is not inherited from Comint mode.
                    ((string= session "*JS REPL*")
                     (require 'indium-repl)
                     (unless (get-buffer session)
                       (indium-run-node org-babel-js-cmd))
                     (indium-eval full-body))
                    ;; session evaluation
                    (t
                     (let ((session (org-babel-prep-session:js
                                     (cdr (assq :session params)) params)))
                       (nth 1
                            (org-babel-comint-with-output
                                (session (format "%S" org-babel-js-eoe) t body)
                              (dolist (code (list body (format "%S" org-babel-js-eoe)))
                                (insert (org-babel-chomp code))
                                (comint-send-input nil t)))))))))
      (org-babel-result-cond (cdr (assq :result-params params))
        result (org-babel-js-read result)))))

(use-package clocker
  :straight (:host github :repo "yqrashawn/clocker.el")
  :disabled
  :custom
  (clocker-skip-after-save-hook-on-mode '(org-mode))
  :bind ("s-j" . clocker-toggle-worklog))

(use-package org-roam
  :straight t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/Dropbox/ORG/")
  (org-roam-graph-viewer "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
  (org-roam-completion-system 'ivy)
  ;; https://org-roam.readthedocs.io/en/latest/templating/
  (org-roam-capture-templates
   '(("d" "default" plain #'org-roam--capture-get-point
      "%?"
      :file-name "%<%Y%m%d%H%M%S>-${slug}"
      :head "#+TITLE: ${title}\n\n#+roam-tags: "
      :unnarrowed t)
     ("z" "zombie" plain #'org-roam--capture-get-point
      "%?"
      :file-name "notes/${slug}"
      :head "#+TITLE: ${title}\n#+DRAFT: true\n#+DATE: %<%Y-%m-%d>"
      :unnarrowed t)
     ("h" "hugo" plain #'org-roam--capture-get-point
      "%?"
      :file-name "notes/${slug}"
      :head "#+title: ${title}\n#+draft: true\n#+date: %<%Y-%m-%d>\n#+hugo_base_dir: ~/site/\n#+hugo_section: notes\n")))
  (org-roam-templates
   (list (list "default" (list :file #'org-roam--file-name-title
                               :content "#+TITLE: ${title}\n\n#+roam-tags: "))))
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n c" . org-roam-capture)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-show-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert)))
  :config
  (require 'org-roam-protocol))

(use-package company-org-roam
  :straight (:host github :repo "jethrokuan/company-org-roam")
  :disabled
  :after (org-roam company)
  :config
  (push 'company-org-roam company-backends)
  (when (boundp 'company-fuzzy-mode)
    (company-fuzzy-mode 0)
    (company-fuzzy-mode 1)))

(use-package deft
  :straight t
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/Dropbox/ORG/")
  :config/el-patch
  (defun deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
If `deft-use-filename-as-title' is nil, the title is taken to
be the first non-empty line of the FILE.  Else the base name of the FILE is
used as title."
    (el-patch-swap (if deft-use-filename-as-title
                       (deft-base-filename file)
                     (let ((begin (string-match "^.+$" contents)))
                       (if begin
                           (funcall deft-parse-title-function
                                    (substring contents begin (match-end 0))))))
                   (org-roam--get-title-or-slug file))))

(defvar yq-org-todo-active-statuses
  '(("TODO" . "t") ("STARTED" . "s") ("BLOCKED" . "b"))
  "List of pairs of active statuses and transition key.")

(use-package org-journal
  :straight t
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-carryover-items (s-join "|"
                                       (-map (lambda (status)
                                               (s-concat "TODO=\"" (s-upcase (car status)) "\""))
                                             yq-org-todo-active-statuses)))

  (org-journal-date-prefix "* ")
  (org-journal-file-format "%Y-%m-%d-%a.org")
  (org-journal-enable-agenda-integration t)
  ;; Sunday, 2020-04-05
  (org-journal-date-format "%A, %Y-%m-%d")
  (org-journal-file-header (lambda (&optional args) (concat "#+TITLE: " (format-time-string org-journal-date-format))))
  (org-journal-dir "~/Dropbox/ORG/")
  :init
  ;; (string-match "\/Dropbox\/ORG\/\d{4}-\d{2}-\d{2}-\w{3}\.org$" "/Users/yqrashawn/Dropbox/ORG/2021-02-09-Tue.org")
  (defun org-journal-find-location ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-reload)
    (org-journal-new-entry t)
    ;; Position point on the journal's top-level heading so that org-capture
    ;; will add the new entry as a child entry.
    (goto-char (point-min))
    ;; first line is title
    (forward-line 1))
  (add-to-list 'org-capture-templates '("j" "Journal entry" entry #'org-journal-find-location
                                        "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?")))

(use-package org-now
  :straight (:host github :repo "alphapapa/org-now")
  :ensure t
  :commands (org-now)
  :disabled
  :bind ("s-j" . +org-now)
  :init
  ;; inspired by clocker.el https://github.com/roman/clocker.el
  (setq org-now-clocker-skip-after-save-hook-on-file-name '("COMMIT_EDITMSG" "recentf"))
  (setq org-now-clocker-skip-after-save-hook-on-extensions '("org"))
  (setq org-now-clocker-skip-after-save-hook-on-modes '())
  (defun org-now-clocker-should-perform-save-hook? (file-name)
    "Check if clocker ignores saves on file with extension file-ext"
    (let ((file-ext (and file-name
                         (file-name-extension file-name))))
      (and
       (not (-contains? org-now-clocker-skip-after-save-hook-on-file-name (file-name-base file-name)))
       (not (-contains? org-now-clocker-skip-after-save-hook-on-extensions file-ext))
       (not (-contains? org-now-clocker-skip-after-save-hook-on-modes (symbol-name major-mode))))))

  (defun +org-journal-entry-header-exists-p (&optional buf)
    "Check if org-journal entry current minute header is exist and edited"
    (unless (boundp 'org-journal-time-prefix) (require 'org-journal))
    (let ((buf (or buf (current-buffer)))
          (header (concat org-journal-time-prefix (format-time-string org-journal-time-format))))
      (with-current-buffer buf
        (goto-char (point-max))
        (let ((header-point (search-backward header nil t)))
          (and header-point (not (eq (- (point-max) (length header)) header-point)))))))

  (defun +org-now-location ()
    (let ((path (org-journal-get-entry-path)))
      (list path (format-time-string org-journal-date-format) org-clock-current-task)))

  (defun +org-now (&optional stay-open no-focus)
    "Function used to toggle or open the clocker buffer.

When there's no org clock running It will enforce user open today's org-journal,
add a new entry.
When there's org clock running, it will simply toggle the org-now buffer on/off.
When STAY-OPEN is t, it won't close the sidebar.
Wehn NO-FOCUS is t, it won't focus to the sidebar."
    (interactive)
    (unless (functionp 'org-clocking-p) (require 'org-clock))
    (when (org-clocking-p) (setq org-now-location (+org-now-location)))
    (let* ((current-wind (get-buffer-window (current-buffer)))
           (org-now-buf (get-buffer "*org-now*"))
           (now-wind (or (and org-now-buf (get-buffer-window org-now-buf))
                         (and (org-clocking-p) (get-buffer-window (org-now-buffer))))))
      (if (org-clocking-p)
          (progn
            ;; (if now-wind
            ;;    (and (not stay-open) (delete-window now-wind))
            ;;  (progn
            ;;    (org-now)
            ;;    (and no-focus (select-window current-wind))))
            )
        ;; don't enforce clock in if I just updated org-journal entry at current minute
        (unless (and org-now-buf (+org-journal-entry-header-exists-p org-now-buf))
          (when (y-or-n-p "Won't save until you clock in, continue?")
            (when now-wind (delete-window now-wind))
            (org-journal-new-entry nil))))))

  (defun org-now-clocker-before-save-hook ()
    (when (org-now-clocker-should-perform-save-hook? (buffer-file-name))
      (+org-now t t)))

  (define-minor-mode org-now-clocker-mode
    "Enable clock-in enforce strategies"
    :lighter " Clocker"
    :global t
    (if org-now-clocker-mode
        (add-hook 'before-save-hook 'org-now-clocker-before-save-hook t)
      (remove-hook 'before-save-hook 'org-now-clocker-before-save-hook)))

  (org-now-clocker-mode 1)
  (global-set-key (kbd "s-j") '+org-now))

(use-package toc-org
  :straight t
  :hook (org-mode . toc-org-mode))

(use-package org-fancy-priorities
  :straight t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

(use-package orgbox
  :straight t
  :commands (orgbox orgbox-schedule)
  :custom
  (orgbox-start-time-of-day "9:30")
  (orgbox-start-time-of-weekends "11:00")
  (orgbox-start-time-of-evening "20:00")
  :init
  (with-eval-after-load 'org
    (evil-define-key 'normal org-mode-map ",ds" #'orgbox-schedule)))

(use-package org-sticky-header
  :straight t
  :hook (org-mode . org-sticky-header-mode))

(use-package company-org-block
  :straight t
  :custom
  (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :hook ((org-mode . (lambda ()
                       (add-to-list (make-local-variable 'company-backends)
                                    'company-org-block)))))