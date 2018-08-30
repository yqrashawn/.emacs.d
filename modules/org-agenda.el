(setq org-agenda-files '("~/Dropbox/ORG/gtd.org"
                         "~/Dropbox/ORG/project.org"))

(setq org-agenda-skip-unavailable-files t)
(with-eval-after-load 'org-agenda
  (setq org-agenda-custom-commands '(("d" "Upcoming deadlines" agenda ""
                                       ((org-agenda-entry-types '(:deadline))
                                        (org-agenda-ndays 1)
                                        (org-deadline-warning-days 30)
                                        (org-agenda-time-grid nil)))
                                     ("w" . "Work stuff")
                                     ("wt" "Agenda and work todo"
                                      ((agenda "") (tags-todo "work")))
                                     ("B" "Todo"
                                      ((tags "OFFICE/TODO")
                                       (tags "HOME/TODO")
                                       (tags "MISC/TODO"))
                                      nil
                                      ("~/agendas/work/todos.txt"))
                                     ("l" "what to do"
                                      ((agenda "" ((org-agenda-ndays 7)
                                                   (org-agenda-entry-types '(:deadline))
                                                   (org-agenda-entry-types '(:scheduled))
                                                   (org-deadline-warning-days 10)))
                                       (tags "OFFICE/TODO")
                                       (tags "HOME/TODO")
                                       (tags "casual/TODO")))
                                     ("ww" "Working Weekly Review"
                                      ((agenda "" ((org-agenda-ndays 7)))
                                       (tags-todo "OFFICE"))
                                      nil
                                      ("~/agendas/week/work.txt"))
                                     ("h" . "Home stuff")
                                     ("ht" "Agenda and home todo"
                                      ((agenda "") (tags-todo "home")))
                                     ("hw" "Home Weekly Review"
                                      ((agenda "" ((org-agenda-ndays 7)))
                                       (tags-todo "home")))
                                     ("c" "casual"
                                      ((tags-todo "casual")))
                                     ("r" "Read later"
                                      ((tags-todo "read"))
                                      nil
                                      ("~/agendas/work/readlater.html")))))