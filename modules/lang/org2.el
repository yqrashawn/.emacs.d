;;; org2.el ---  org packages -*- lexical-binding: t; -*-
(with-eval-after-load 'org
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
    ",xv" (spacemacs|org-emphasize spacemacs/org-verbatim ?=)))

(use-package ob
  :defer t
  :init
  (setq org-babel-load-languages
        '((emacs-lisp . t)
          (sql . t)
          (sql-mode . t)
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

(use-package ob-mongo
  :straight t
  :after ob)