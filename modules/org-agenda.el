(setq org-agenda-files (list "~/Dropbox/ORG"))
(setq org-agenda-skip-unavailable-files t)
(with-eval-after-load 'org-agenda
  (add-to-list 'org-agenda-custom-commands
               '("d" "Upcoming deadlines" agenda ""
                 ((org-agenda-entry-types '(:deadline))
                  (org-agenda-ndays 1)
                  (org-deadline-warning-days 30)
                  (org-agenda-time-grid nil))))
  (add-to-list 'org-agenda-custom-commands '("w" . "Work stuff"))
  (add-to-list 'org-agenda-custom-commands '("wt" "Agenda and work todo" ((agenda "") (tags-todo "work"))))
  (add-to-list 'org-agenda-custom-commands
               '("B" "Todo"
                 ((tags "OFFICE/TODO")
                  (tags "HOME/TODO")
                  (tags "MISC/TODO"))
                 nil
                 ("~/agendas/work/todos.pdf"
                  "~/agendas/work/todos.csv"
                  "~/agendas/work/todos.txt"
                  "~/agendas/work/todos.html")))
  (add-to-list 'org-agenda-custom-commands
               '("l" "what to do"
                 ((agenda "" ((org-agenda-ndays 7)
                              (org-agenda-entry-types '(:deadline))
                              (org-agenda-entry-types '(:scheduled))
                              (org-deadline-warning-days 10))) ;; review upcoming deadlines and appointments
                  ;; type "l" in the agenda to review logged items
                  ;; (stuck "OFFICE" ((org-agenda-files (org-projectile-todo-files)))) ;; review stuck projects
                  ;; (stuck "HOME" ((org-agenda-files (org-projectile-todo-files)))) ;; review stuck projects
                  (tags "OFFICE/TODO")
                  (tags "HOME/TODO")
                  (tags "casual/TODO"))
                 nil
                 ("~/agendas/week/work.pdf"
                  "~/agendas/week/work.csv"
                  "~/agendas/week/work.txt"
                  "~/agendas/week/work.html")))
  (add-to-list 'org-agenda-custom-commands
               '("ww" "Working Weekly Review"
                 ((agenda "" ((org-agenda-ndays 7))) ;; review upcoming deadlines and appointments
                  ;; type "l" in the agenda to review logged items
                  ;; (stuck "OFFICE" ((org-agenda-files (org-projectile-todo-files)))) ;; review stuck projects
                  (tags-todo "OFFICE")) nil ("~/agendas/week/work.pdf" "~/agendas/week/work.csv" "~/agendas/week/work.txt" "~/agendas/week/work.html"))) ;; review waiting items
  (add-to-list 'org-agenda-custom-commands
               '("wp" . "Working Priority")) ;; review waiting items
  (add-to-list 'org-agenda-custom-commands
               '("wpa" "Working Priority A" tags-todo "+PRIORITY=\"A\"+work")) ;; review waiting items
  (add-to-list 'org-agenda-custom-commands '("h" . "Home stuff"))
  (add-to-list 'org-agenda-custom-commands '("ht" "Agenda and home todo" ((agenda "") (tags-todo "home"))))
  (add-to-list 'org-agenda-custom-commands
               '("hw" "Home Weekly Review"
                 ((agenda "" ((org-agenda-ndays 7))) ;; review upcoming deadlines and appointments
                  (tags-todo "home"))))
  (add-to-list 'org-agenda-custom-commands
               '("c" "casual"
                 ((tags-todo "casual"))))
  (add-to-list 'org-agenda-custom-commands '("r" "Read later" ((tags-todo "read")) nil ("~/agendas/work/readlater.html" "~/agendas/work/readlater.txt"))))