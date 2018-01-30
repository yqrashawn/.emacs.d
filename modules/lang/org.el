(use-package org
  :straight t
  :defer t
  :commands (orgtbl-mode)
  :init
  (customize-set-variable 'org-startup-indented t)
  (setq org-clock-persist-file (concat spacemacs-cache-directory
                                       "org-clock-save.el")
        org-id-locations-file (concat spacemacs-cache-directory
                                      ".org-id-locations")
        org-publish-timestamp-directory (concat spacemacs-cache-directory
                                                ".org-timestamps/")
        org-directory "~/org" ;; needs to be defined for `org-default-notes-file'
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-log-done t
        org-startup-with-inline-images t
        org-image-actual-width nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        ;; this is consistent with the value of
        ;; `helm-org-headings-max-depth'.
        org-imenu-depth 8)

  (with-eval-after-load 'org-indent
    (diminish 'org-indent-mode))
  (defmacro spacemacs|org-emphasize (fname char)
    "Make function for setting the emphasis in org mode"
    `(defun ,fname () (interactive)
            (org-emphasize ,char)))

  (with-eval-after-load 'org-src
    (evil-define-key 'org-src-mode
      ",c" 'org-edit-src-exit
      ",a" 'org-edit-src-abort
      ",k" 'org-edit-src-abort))

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

  (dolist (prefix '(
                    ("mC" . "clocks")
                    ("md" . "dates")
                    ("me" . "export")
                    ("mh" . "headings")
                    ("mi" . "insert")
                    ("miD" . "download")
                    ("ms" . "trees/subtrees")
                    ("mT" . "toggles")
                    ("mt" . "tables")
                    ("mtd" . "delete")
                    ("mti" . "insert")
                    ("mtt" . "toggle")
                    ("mx" . "text"))))
  (evil-define-key 'normal org-mode-map
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
    ;; region manipulation
    ",xb" (spacemacs|org-emphasize spacemacs/org-bold ?*)
    ",xc" (spacemacs|org-emphasize spacemacs/org-code ?~)
    ",xi" (spacemacs|org-emphasize spacemacs/org-italic ?/)
    ",xo" 'org-open-at-point
    ",xr" (spacemacs|org-emphasize spacemacs/org-clear ? )
    ",xs" (spacemacs|org-emphasize spacemacs/org-strike-through ?+)
    ",xu" (spacemacs|org-emphasize spacemacs/org-underline ?_)
    ",xv" (spacemacs|org-emphasize spacemacs/org-verbatim ?=))

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

  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (define-key global-map "\C-cc" 'org-capture)
  :config
  (add-to-list 'org-babel-load-languages '(shell . t))
  ;; We add this key mapping because an Emacs user can change
  ;; `dotspacemacs-major-mode-emacs-leader-key' to `C-c' and the key binding
  ;; C-c ' is shadowed by `spacemacs/default-pop-shell', effectively making
  ;; the Emacs user unable to exit src block editing.
  (define-key org-src-mode-map
    (kbd (concat "," " '"))
    'org-edit-src-exit)

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

(use-package org-capture
  :defer
  :commands (org-capture)
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
          ("l" "Capture from the Internet with link" entry
           (file+olp "~/Dropbox/ORG/notes.org" "notes" "read")
           "*** TODO %? %^G\n%U")
          ("b" "Brain" plain (function org-brain-goto-end)
           "* %i%?\n")
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

(use-package evil-org
  :straight t
  :defer t
  :diminish evil-org-mode
  :init
  ;; (add-hook 'org-mode-hook 'spacemacs//evil-org-mode)
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

(use-package org-projectile
  :straight t
  :config
  (setq org-projectile-capture-template "* TODO %? %^G\n%U"))

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

(yq/get-modules "org-agenda.el")