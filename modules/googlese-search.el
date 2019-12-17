;;; googlese-search.el --- google and stackexchange integration  -*- lexical-binding: t; -*-

(require 'json)
(require 'cl)
(require 'subr-x)

(defun googlese-get-completions (query callback)
  (url-retrieve (format "http://suggestqueries.google.com/complete/search?client=chrome&q=%s" query)
                callback))

(setq googlese-bufname "*stackexchange-suggestions*")

(setq googlese-keymap
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map minibuffer-local-map)
        (define-key map (kbd "C-k") 'googlese-prev)
        (define-key map (kbd "C-j") 'googlese-next)
        (define-key map (kbd "C-l") 'exit-minibuffer)
        map))

(setq googlese-input nil)

;;;###autoload
(defun googlese-search ()
  (interactive)
  (let ((wincfg (current-window-configuration)))
    (pop-to-buffer googlese-bufname)
    (erase-buffer)
    (setq cursor-type nil)
    (add-hook 'post-command-hook 'googlese-post-command)
    (setq googlese-input nil)
    (if (unwind-protect
            (progn
              (read-from-minibuffer "search for: " nil googlese-keymap)
              (and googlese-input
                   (not (equal googlese-input ""))))

          (remove-hook 'post-command-hook 'googlese-post-command)
          (set-window-configuration wincfg))

        (my-get-stackoverflow-answers googlese-input))))

(defun googlese-post-command ()
  (if (and (not (equal googlese-input (minibuffer-contents)))
           (sit-for 0.3))

      (if (equal (minibuffer-contents) "")
          (with-current-buffer (get-buffer googlese-bufname)
            (erase-buffer))

        (let ((input (minibuffer-contents)))
          (googlese-get-completions
           input
           (lambda (status &rest args)
             (unless status
               (search-forward "\n\n")
               (let ((suggestions (append (aref (json-read) 1) nil)))
                 (with-current-buffer (get-buffer googlese-bufname)
                   (erase-buffer)
                   (insert input "\n")
                   (dolist (suggestion suggestions)
                     (insert suggestion "\n"))
                   (goto-char (point-min))
                   (googlese-highlight-line)
                   (setq googlese-input input))))))))))

(defun googlese-highlight-line ()
  (put-text-property (line-beginning-position)
                     (1+ (line-end-position))
                     'face 'show-paren-match))

(defun googlese-clear-highlight ()
  (put-text-property (line-beginning-position)
                     (1+ (line-end-position))
                     'face
                     'nil))

(defun googlese-next ()
  (interactive)
  (with-current-buffer (get-buffer googlese-bufname)
    (unless (or (eobp)
                (save-excursion
                  (forward-line 1)
                  (eobp)))
      (googlese-clear-highlight)
      (forward-line 1)
      (googlese-item-selected))))

(defun googlese-prev ()
  (interactive)
  (with-current-buffer (get-buffer googlese-bufname)
    (unless (bobp)
      (googlese-clear-highlight)
      (forward-line -1)
      (googlese-item-selected))))

(defun googlese-item-selected ()
  (let ((item (with-current-buffer (get-buffer googlese-bufname)
                (googlese-highlight-line)
                (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position)))))
    (select-window (minibuffer-window))
    (delete-minibuffer-contents)
    (insert item)
    (setq googlese-input item)))

(defun my-get-stackoverflow-answers (query)
  (interactive "sQuestion: ")
  (let* ((question_ids
          (with-current-buffer
              (url-retrieve-synchronously
               (concat "https://google.com/search?ie=utf-8&oe=utf-8&hl=en&as_qdr=all&q="
                       (url-hexify-string (concat query " site:stackoverflow.com"))))
            (let (ids)
              (while (re-search-forward "https://stackoverflow.com/questions/\\([0-9]+\\)" nil t)
                (push (match-string-no-properties 1) ids))
              (setq ids (reverse ids))
              (if (> (length ids) 5)
                  (subseq ids 0 5)
                ids))))

         (url_template (format "https://api.stackexchange.com/2.2/questions/%s%%s?site=stackoverflow.com"
                               (string-join question_ids ";")))

         (questions (with-current-buffer
                        (url-retrieve-synchronously
                         (format url_template ""))
                      (goto-char (point-min))
                      (search-forward "\n\n")
                      (append (assoc-default 'items (json-read)) nil)))

         (answers (with-current-buffer
                      (url-retrieve-synchronously
                       (concat (format url_template "/answers")
                               "&order=desc&sort=activity&filter=withbody"))
                    (goto-char (point-min))
                    (search-forward "\n\n")
                    (sort (append (assoc-default 'items (json-read)) nil)
                          (lambda (x y)
                            (> (assoc-default 'score x)
                               (assoc-default 'score y)))))))

    (switch-to-buffer "*stackexchange*")
    (setq buffer-read-only nil)
    (erase-buffer)

    (dolist (question_id (mapcar 'string-to-number question_ids))
      (let ((question (some (lambda (question)
                              (if (equal (assoc-default 'question_id question)
                                         question_id)
                                  question))
                            questions)))
        (insert "<hr><h2 style='background-color:paleturquoise'>Question: "
                (format "<a href='%s'>%s</a> - %s"
                        (assoc-default 'link question)
                        (assoc-default 'title question)
                        (format-time-string
                         "%Y-%m-%d"
                         (or (assoc-default 'last_edit_date question)
                             (assoc-default 'creation_date question))))
                "</h2>"
                "\n"
                (mapconcat
                 'identity
                 (let ((rendered
                        (remove-if
                         'null
                         (mapcar (lambda (answer)
                                   (if (and (equal question_id
                                                   (assoc-default 'question_id answer))
                                            (>= (assoc-default 'score answer) 0))
                                       (concat "<hr><h2 style='background-color:"
                                               "#c1ffc1'>Answer - score: "
                                               (number-to-string (assoc-default 'score answer))
                                               " - "
                                               (format-time-string
                                                "%Y-%m-%d"
                                                (or (assoc-default 'last_edit_date answer)
                                                    (assoc-default 'creation_date answer)))
                                               "</h2>"
                                               (assoc-default 'body answer))))
                                 answers))))
                   (if (> (length rendered) 5)
                       (append (subseq rendered 0 5)
                               (list (format "<br><br><a href='%s'>%s</a>"
                                             (assoc-default 'link question)
                                             "More answers...")))
                     rendered))
                 "\n"))))

    (shr-render-region (point-min) (point-max))
    (goto-char (point-min))
    (save-excursion
      (while (search-forward "^M" nil t)
        (replace-match "")))
    (setq buffer-read-only t)))

(provide 'googlese-search)
;;; googlese-search.el ends here