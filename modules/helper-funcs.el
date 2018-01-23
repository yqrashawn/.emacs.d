(defun align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))
(defun edit-current-file-as-root ()
  "Edit as root the file associated with the current buffer"
  (interactive)
  (if (buffer-file-name)
      (progn
        (setq file (concat "/sudo:root@localhost:" (buffer-file-name)))
        (find-file file))
    (message "Buffer is not associated to a file.")))
(defun move-file (new-location)
  "Write this file to NEW-LOCATION, and delete the old one."
  (interactive (list (if buffer-file-name
                         (read-file-name "Move file to: ")
                       (read-file-name "Move file to: "
                                       default-directory
                                       (expand-file-name (file-name-nondirectory (buffer-name))
                                                         default-directory)))))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (buffer-file-name)))
    (write-file new-location t)
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
      (delete-file old-location))))

(defvar my/move-file-here-start-dir (expand-file-name "~/"))
(defun my/move-file-here ()
  "Move file from somewhere else to here.

The file is taken from a start directory set by
`my/move-file-here-start-dir' and moved to the current directory
if invoked in dired, or else the directory containing current
buffer. The user is presented with a list of files in the start
directory, from which to select the file to move, sorted by most
recent first.

The short filename is copied to clipboard.

Quickly move a file to the current directory | Pragmatic Emacs
http://pragmaticemacs.com/emacs/quickly-move-a-file-to-the-current-directory/
"
  (interactive)
  (let (file-list target-dir file-list-sorted start-file start-file-full)
    ;; clean directories from list but keep times
    (setq file-list
          (-remove (lambda (x) (nth 1 x))
                   (directory-files-and-attributes my/move-file-here-start-dir)))

    ;; get target directory
    ;; http://ergoemacs.org/emacs/emacs_copy_file_path.html
    (setq target-dir
          (if (equal major-mode 'dired-mode)
              (expand-file-name default-directory)
            (if (null (buffer-file-name))
                (user-error "ERROR: current buffer is not associated with a file.")
              (file-name-directory (buffer-file-name)))))

    ;; sort list by most recent
    ;; http://stackoverflow.com/questions/26514437/emacs-sort-list-of-directories-files-by-modification-date
    (setq file-list-sorted
          (mapcar #'car
                  (sort file-list
                        #'(lambda (x y) (time-less-p (nth 6 y) (nth 6 x))))))

    ;; use ivy to select start-file
    (setq start-file (ivy-read
                      (concat "Move selected file to " target-dir ":")
                      file-list-sorted
                      :re-builder #'ivy--regex
                      :sort nil
                      :initial-input nil))

    ;; add full path to start file and end-file
    (setq start-file-full
          (expand-file-name start-file my/move-file-here-start-dir))
    (setq end-file
          (expand-file-name (file-name-nondirectory start-file) target-dir))
    (rename-file start-file-full end-file)
    ;; copy short filename to clipboard
    (kill-new start-file)
    (gui-set-selection 'PRIMARY start-file)
    (message "moved %s to %s" start-file-full end-file)))
o
(defun yq/start-scratch-html-file (file-name)
  "Create a test HTML file in ~/Downloads/scratch/FILE-NAME to play around with."
  (interactive "sName of scratch HTML file: ")
  (yq/start-scratch-file (format "%s.html" file-name))
  (erase-buffer)
  (insert "<!DOCTYPE html>
<html>
    <head>
        <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">
        <title>
        </title>
        <style type=\"text/css\">
        </style>
    </head>
    <body>

    </body>
</html>")
  (forward-line -2)
  (move-end-of-line nil))



(defun yq/copy-file-path-to-clipboard ()
  "Put the current file name into the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))