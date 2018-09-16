(setq org-agenda-files '("~/Dropbox/ORG/gtd.org"
                         "~/Dropbox/ORG/project.org"))

(setq org-agenda-skip-unavailable-files t)

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
            (stuck ""
                   ((org-agenda-overriding-header "Stucked Projects")
                    (org-stuck-projects
                     '("+LEVEL=1/-DONE"
                       ("TODO" "NEXT" "SOMEDAY" "PRIORITY=\"C\"")
                       ("NOSTUCK")
                       ""))
                    (org-agenda-files
                     '("~/Dropbox/ORG/project.org"))))
            (stuck ""
                   ((org-agenda-overriding-header "Stucked Todos")
                    (org-stuck-projects
                     '("+LEVEL=4/-DONE"
                       ("TODO" "NEXT" "SOMEDAY" "PRIORITY=\"C\"")
                       ("NOSTUCK")
                       ""))
                    (org-agenda-files
                     '("~/Dropbox/ORG/gtd.org"))))
            (tags-todo "+OFFICE-PRIORITY=\"C\""
                       ((org-agenda-overriding-header "Office High Priority Tasks")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'todo 'done))))
            (tags-todo "HOME-PRIORITY=\"C\""
                       ((org-agenda-overriding-header "Home High Priority Tasks")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'todo 'done))))
            (tags-todo "+CREATED>=\"<-1w>\"|+UPDATED>=\"<-1w>\""
                       ((org-agenda-overriding-header "Tasks Created This Week")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'todo 'done))))
            (tags "*"
                  ((org-agenda-overriding-header "Recent Activity")
                   (org-agenda-skip-function '(+org/has-child-and-last-update-before 7)))))
           nil nil)
          ("B" "Todo"
           ((tags "OFFICE/TODO" nil)
            (tags "HOME/TODO" nil)
            (tags "MISC/TODO" nil))
           nil
           ("~/agendas/work/todos.txt")))))

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
  ("j" org-agenda-next-line)
  ("k" org-agenda-previous-line)
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

(define-key org-agenda-mode-map (kbd ".") 'hydra-org-agenda/body)
;; (add-hook 'org-agenda-mode-hook 'hydra-org-agenda/body)