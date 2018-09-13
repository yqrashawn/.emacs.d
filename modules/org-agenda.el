(setq org-agenda-files '("~/Dropbox/ORG/gtd.org"
                         "~/Dropbox/ORG/project.org"))

(setq org-agenda-skip-unavailable-files t)
(with-eval-after-load 'org-agenda
  (setq org-agenda-custom-commands
        '(("r" "Review"
           ((agenda ""
                    ((org-agenda-overriding-header "\"Agenda\"")
                     (org-agenda-include-diary t)
                     (org-agenda-span 'week)
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'nottodo 'done))))
            (stuck ""
                   ((org-agenda-overriding-header "Stucked")))
            (tags-todo "OFFICE-PRIORITY=\"C\"|HOME-PRIORITY=\"C\""
                       ((org-agenda-overriding-header "What to do now?")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'todo 'done))
                        (org-agenda-tag-filter-preset 'nil))))
           nil nil)
          ("d" "Upcoming deadlines" agenda ""
           ((org-agenda-entry-types
             '(:deadline))
            (org-agenda-ndays 1)
            (org-deadline-warning-days 30)
            (org-agenda-time-grid nil)))
          ("w" . "Work stuff")
          ("wt" "Agenda and work todo"
           ((agenda "" nil)
            (tags-todo "work" nil))
           nil)
          ("B" "Todo"
           ((tags "OFFICE/TODO" nil)
            (tags "HOME/TODO" nil)
            (tags "MISC/TODO" nil))
           nil
           ("~/agendas/work/todos.txt"))
          ("l" "what to do"
           ((agenda ""
                    ((org-agenda-ndays 7)
                     (org-agenda-entry-types
                      '(:deadline))
                     (org-agenda-entry-types
                      '(:scheduled))
                     (org-deadline-warning-days 10)))
            (tags "OFFICE/TODO" nil)
            (tags "HOME/TODO" nil)
            (tags "casual/TODO" nil))
           nil)
          ("ww" "Working Weekly Review"
           ((agenda ""
                    ((org-agenda-ndays 7)))
            (tags-todo "OFFICE" nil))
           nil
           ("~/agendas/week/work.txt"))
          ("h" . "Home stuff")
          ("ht" "Agenda and home todo"
           ((agenda "" nil)
            (tags-todo "home" nil))
           nil)
          ("hw" "Home Weekly Review"
           ((agenda ""
                    ((org-agenda-ndays 7)))
            (tags-todo "home" nil))
           nil)
          ("c" "casual"
           ((tags-todo "casual" nil))
           nil)
          ("r" "Read later"
           ((tags-todo "read" nil))
           nil
           ("~/agendas/work/readlater.html")))))