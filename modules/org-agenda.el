(setq org-agenda-files '("~/Dropbox/ORG/gtd.org"
                         "~/Dropbox/ORG/project.org"))

(setq org-agenda-skip-unavailable-files t)
(with-eval-after-load 'org-agenda
  (setq org-agenda-custom-commands
        '(("r" "Review"
           ((agenda ""
                    ((org-agenda-ndays 7)
                     (org-agenda-include-diary t)
                     (org-agenda-span 'week)
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'done))))
            (stuck ""
                   ((org-agenda-overriding-header "Stucked")))
            (tags-todo "+OFFICE-PRIORITY=\"C\""
                       ((org-agenda-overriding-header "Office High Priority Tasks")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'todo 'done))))
            (tags-todo "HOME-PRIORITY=\"C\""
                       ((org-agenda-overriding-header "Home High Priority Tasks")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'todo 'done))))
            (tags-todo "+CREATED>=\"<-1w>\"|+UPDATED>=\"<-1w>\""
                       ((org-agenda-overriding-header "Tasks Created This Weed")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'todo 'done)))))
           nil nil)
          ("B" "Todo"
           ((tags "OFFICE/TODO" nil)
            (tags "HOME/TODO" nil)
            (tags "MISC/TODO" nil))
           nil
           ("~/agendas/work/todos.txt")))))