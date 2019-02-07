(setq org-agenda-skip-unavailable-files t)
(setq org-agenda-files (expand-file-name "agendafile" org-directory))

(defun +org/has-child-p ()
  (save-excursion (org-goto-first-child)))

(defun +org/has-child-and-last-update-before (day)
  (if (+org/has-child-p) (point)
    (+org/last-update-before day)))

(defun yant/getentryhash ()
  "Get the hash sum of the text in current entry, except :HASH: and :MODIFIED: property texts."
  (save-excursion
    (let* ((beg (progn (org-back-to-heading) (point)))
           (end (progn
                  (forward-char)
                  (if (not (re-search-forward "^\*+ " (point-max) t))
                      (point-max)
                    (match-beginning 0))))
           (full-str (buffer-substring beg end))
           (str-nohash (if (string-match "^ *:HASH:.+\n" full-str)
                           (replace-match "" nil nil full-str)
                         full-str))
           (str-nohash-nomod (if (string-match "^ *:MODIFIED:.+\n" str-nohash)
                                 (replace-match "" nil nil str-nohash)
                               str-nohash))
           (str-nohash-nomod-nopropbeg (if (string-match "^ *:PROPERTIES:\n" str-nohash-nomod)
                                           (replace-match "" nil nil str-nohash-nomod)
                                         str-nohash-nomod))
           (str-nohash-nomod-nopropbeg-end (if (string-match "^ *:END:\n" str-nohash-nomod-nopropbeg)
                                               (replace-match "" nil nil str-nohash-nomod-nopropbeg)
                                             str-nohash-nomod-nopropbeg)))
      (sxhash str-nohash-nomod-nopropbeg-end))))


(defun yant/update-modification-time ()
  "Set the :MODIFIED: property of the current entry to NOW and update :HASH: property."
  (org-set-property "HASH" (format "%s" (yant/getentryhash))) (org-set-property "MODIFIED" (format-time-string "%Y-%m-%d %H:%M")))

(defun yant/skip-nonmodified ()
  "Skip org entries, which were not modified according to the :HASH: property"
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
    (if (string= (org-entry-get (point) "HASH" nil) (format "%s" (yant/getentryhash))))
    next-headline nil))

(with-eval-after-load 'org-agenda
  (setq org-agenda-custom-commands
        '(("r" "Review"
           ((agenda ""
                    ((org-agenda-ndays 7)
                     (org-agenda-include-diary t)
                     (org-agenda-span 'week)
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'done))))
            ;; (stuck ""
            ;;        ((org-agenda-overriding-header "Stucked Projects")
            ;;         (org-stuck-projects
            ;;          '("+LEVEL=1/-DONE"
            ;;            ("TODO" "NEXT" "SOMEDAY" "PRIORITY=\"C\"")
            ;;            ("NOSTUCK")
            ;;            ""))
            ;;         (org-agenda-files
            ;;          '("~/Dropbox/ORG/project.org"))))
            ;; (stuck ""
            ;;        ((org-agenda-overriding-header "Stucked Todos")
            ;;         (org-stuck-projects
            ;;          '("+LEVEL=4/-DONE"
            ;;            ("TODO" "NEXT" "SOMEDAY" "PRIORITY=\"C\"")
            ;;            ("NOSTUCK")
            ;;            ""))
            ;;         (org-agenda-files
            ;;          '("~/Dropbox/ORG/gtd.org"))))
            ;; (tags-todo "+OFFICE-PRIORITY=\"C\""
            ;;            ((org-agenda-overriding-header "Office High Priority Tasks")
            ;;             (org-agenda-skip-function
            ;;              '(org-agenda-skip-entry-if 'todo 'done))))
            ;; (tags-todo "HOME-PRIORITY=\"C\""
            ;;            ((org-agenda-overriding-header "Home High Priority Tasks")
            ;;             (org-agenda-skip-function
            ;;              '(org-agenda-skip-entry-if 'todo 'done))))
            (tags-todo "+CREATED>=\"<-1w>\"|+UPDATED>=\"<-1w>\""
                       ((org-agenda-overriding-header "Tasks Created This Week")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'todo 'done)))))
           ;; (tags "*"
           ;;       ((org-agenda-overriding-header "Recent Activity")
           ;;        (org-agenda-skip-function '(+org/has-child-and-last-update-before 7)))))
           nil nil)
          ("B" "Todo"
           ((tags "OFFICE/TODO" nil)
            (tags "HOME/TODO" nil)
            (tags "MISC/TODO" nil))
           nil
           ("~/agendas/work/todos.txt")))))

