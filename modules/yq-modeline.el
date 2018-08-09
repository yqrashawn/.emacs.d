(use-package shrink-path
  :straight t
  :commands (shrink-path-file-mixed))

(defun yq/fancy-mode-line-render (left center right &optional lpad rpad)
  "Return a string the width of the current window with
LEFT, CENTER, and RIGHT spaced out accordingly, LPAD and RPAD,
can be used to add a number of spaces to the front and back of the string."
  (condition-case err
      (let* ((left (if lpad (concat (make-string lpad ?\s) left) left))
             (right (if rpad (concat right (make-string rpad ?\s)) right))
             (width (apply '+ (window-width) (let ((m (window-margins))) (list (or (car m) 0) (or (cdr m) 0)))))
             (total-length (+ (length left) (length center) (length right) 2)))
        (when (> total-length width) (setq left "" right ""))
        (let* ((left-space (/ (- width (length center)) 2))
               (right-space (- width left-space (length center)))
               (lspaces (max (- left-space (length left)) 1))
               (rspaces (max (- right-space (length right)) 1 0)))
          (concat left (make-string lspaces  ?\s)
                  center
                  (make-string rspaces ?\s)
                  right)))
    (error (format "[%s]: (%s) (%s) (%s)" err left center right))))

(defun test-mode-line ()
  (interactive)
  (defvar fancy-buffer-file-name (string-join
                                  (last (shrink-path-file-mixed
                                         (f-slash
                                          (f-short
                                           (projectile-project-root)))
                                         (file-name-directory
                                          (or buffer-file-truename
                                              (file-truename buffer-file-name))) (file-truename buffer-file-name)) 2)))

  (defsubst active ()
    (eq (selected-window) (frame-selected-window)))
  (setq fancy-vcs (format-mode-line (when (and vc-mode buffer-file-name)
                                        (let* ((backend (vc-backend buffer-file-name))
                                               (state   (vc-state buffer-file-name backend)))
                                          (let ((face    'mode-line-inactive)
                                                (active  (active))
                                                (all-the-icons-scale-factor 1.0)
                                                (all-the-icons-default-adjust -0.1))
                                            (concat "  "
                                                    (cond ((memq state '(edited added))
                                                           (if active (setq face 'doom-modeline-info))
                                                           (all-the-icons-octicon
                                                            "git-compare"
                                                            :face face
                                                            :height 1.2
                                                            :v-adjust -0.05))
                                                          ((eq state 'needs-merge)
                                                           (if active (setq face 'doom-modeline-info))
                                                           (all-the-icons-octicon "git-merge" :face face))
                                                          ((eq state 'needs-update)
                                                           (if active (setq face 'doom-modeline-warning))
                                                           (all-the-icons-octicon "arrow-down" :face face))
                                                          ((memq state '(removed conflict unregistered))
                                                           (if active (setq face 'doom-modeline-urgent))
                                                           (all-the-icons-octicon "alert" :face face))
                                                          (t
                                                           (if active (setq face 'font-lock-doc-face))
                                                           (all-the-icons-octicon
                                                            "git-compare"
                                                            :face face
                                                            :height 1.2
                                                            :v-adjust -0.05)))
                                                    " "
                                                    (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                                                                'face (if active face))
                                                    " "))))))
  (setq-local mode-line-format
              '((:eval (yq/fancy-mode-line-render
                        (string-join (list evil-mode-line-tag magit-buffer-refname magit-buffer-file-name fancy-buffer-file-name))
                        "center"
                        (string-join (list fancy-vcs mode-name))
                        0
                        0))))
  (force-mode-line-update))

(spacemacs/set-leader-keys "o" 'test-mode-line)
(global-set-key (kbd "C-c C-l") 'test-mode-line)