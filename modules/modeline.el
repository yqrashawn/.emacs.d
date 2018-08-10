(require 'all-the-icons)

(defgroup +doom-modeline nil
  ""
  :group 'doom)

(defface doom-modeline-buffer-path
  '((t (:inherit mode-line-emphasis :bold t)))
  "Face used for the dirname part of the buffer path."
  :group '+doom-modeline)

(defface doom-modeline-buffer-modified
  '((t (:inherit error :background nil :bold t)))
  "Face used for the 'unsaved' symbol in the mode-line."
  :group '+doom-modeline)

(defface doom-modeline-buffer-major-mode
  '((t (:inherit mode-line-emphasis :bold t)))
  "Face used for the major-mode segment in the mode-line."
  :group '+doom-modeline)

(defface doom-modeline-panel
  '((t (:inherit mode-line-highlight)))
  "Face for 'X out of Y' segments, such as `+doom-modeline--anzu', `+doom-modeline--evil-substitute' and
`iedit'"
  :group '+doom-modeline)

(defface doom-modeline-info
  `((t (:inherit success :bold t)))
  "Face for info-level messages in the modeline. Used by `*vc'."
  :group '+doom-modeline)

(defface doom-modeline-warning
  `((t (:inherit warning :bold t)))
  "Face for warnings in the modeline. Used by `*flycheck'"
  :group '+doom-modeline)

(defface doom-modeline-urgent
  `((t (:inherit error :bold t)))
  "Face for errors in the modeline. Used by `*flycheck'"
  :group '+doom-modeline)

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

(defun fancy-buffer-name ()
  (if (buffer-file-name)
      (string-join
       (last (shrink-path-file-mixed
              (f-slash
               (f-short
                (projectile-project-root)))
              (file-name-directory
               (or buffer-file-truename
                   (file-truename buffer-file-name))) (file-truename buffer-file-name)) 2))
    (buffer-name)))

(defsubst active ()
  (eq (selected-window) (frame-selected-window)))

(defun fancy-major-mode ()
  (format-mode-line (propertize
                     (concat (format-mode-line mode-name)
                             (when (stringp mode-line-process)
                               mode-line-process)
                             (and (featurep 'face-remap)
                                  (/= text-scale-mode-amount 0)
                                  (format " (%+d)" text-scale-mode-amount)))
                     'face (if (active) 'doom-modeline-buffer-major-mode))))

(defun fancy-vcs ()
  (format-mode-line (when (and vc-mode buffer-file-name)
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

(defun fancy-project ()
  (format-mode-line (let ((face (if (active) 'doom-modeline-buffer-path)))
                      (concat (propertize (concat (abbreviate-file-name (projectile-project-root)))
                                          'face face)))))

(defun fancy-center ()
  (if (buffer-file-name)
      (concat (fancy-project) (fancy-buffer-name))
    (buffer-name)))

(defun fancy-buffer-info ()
  (format-mode-line
   (let ((all-the-icons-scale-factor 1.2))
     (concat (cond (buffer-read-only
                    (concat (all-the-icons-octicon
                             "lock"
                             :face 'doom-modeline-warning
                             :v-adjust -0.05)
                            " "))
                   ((buffer-modified-p)
                    (concat (all-the-icons-faicon
                             "floppy-o"
                             :face 'doom-modeline-buffer-modified
                             :v-adjust -0.0575)
                            " "))
                   ((and buffer-file-name
                         (not (file-exists-p buffer-file-name)))
                    (concat (all-the-icons-octicon
                             "circle-slash"
                             :face 'doom-modeline-urgent
                             :v-adjust -0.05)
                            " "))
                   ((buffer-narrowed-p)
                    (concat (all-the-icons-octicon
                             "fold"
                             :face 'doom-modeline-warning
                             :v-adjust -0.05)
                            " ")))))))

;; (defun fancy-persp ()
;;   (if (bound-and-true-p persp-mode)
;;       (safe-persp-name (get-frame-persp))
;;     ""))

(defvar +doom-modeline-vspc
  (propertize " " 'face 'variable-pitch)
  "TODO")

(defun +doom-ml-icon (icon &optional text face voffset)
  "Displays an octicon ICON with FACE, followed by TEXT. Uses
`all-the-icons-octicon' to fetch the icon."
  (concat (if vc-mode " " "  ")
          (when icon
            (concat
             (all-the-icons-material icon :face face :height 1.1 :v-adjust (or voffset -0.2))
             (if text +doom-modeline-vspc)))
          (when text
            (propertize text 'face face))
          (if vc-mode "  " " ")))

(defun fancy-flycheck ()
  (format-mode-line
   (when (boundp 'flycheck-last-status-change)
     (pcase flycheck-last-status-change
       ('finished (if flycheck-current-errors
                      (let-alist (flycheck-count-errors flycheck-current-errors)
                        (let ((sum (+ (or .error 0) (or .warning 0))))
                          (+doom-ml-icon "do_not_disturb_alt"
                                         (number-to-string sum)
                                         (if .error 'doom-modeline-urgent 'doom-modeline-warning)
                                         -0.25)))
                    (+doom-ml-icon "check" nil 'doom-modeline-info)))
       ('running     (+doom-ml-icon "access_time" nil 'font-lock-doc-face -0.25))
       ('no-checker  (+doom-ml-icon "sim_card_alert" "-" 'font-lock-doc-face))
       ('errored     (+doom-ml-icon "sim_card_alert" "Error" 'doom-modeline-urgent))
       ('interrupted (+doom-ml-icon "pause" "Interrupted" 'font-lock-doc-face))))))

(defun fancy-macro-recoding ()
  (format-mode-line
   (when (and (active) (or defining-kbd-macro executing-kbd-macro))
     (let ((sep (propertize " " 'face 'doom-modeline-panel)))
       (concat sep
               (propertize (if (bound-and-true-p evil-this-macro)
                               (char-to-string evil-this-macro)
                             "Macro")
                           'face 'doom-modeline-panel)
               sep
               (all-the-icons-octicon "triangle-right"
                                      :face 'doom-modeline-panel
                                      :v-adjust -0.05)
               sep)))))

(defvar anzu--state nil)
(defvar iedit-mode nil)
(defun fancy-anzu ()
  (format-mode-line
   (when (and anzu--state (not iedit-mode))
     (propertize
      (let ((here anzu--current-position)
            (total anzu--total-matched))
        (cond ((eq anzu--state 'replace-query)
               (format " %d replace " total))
              ((eq anzu--state 'replace)
               (format " %d/%d " here total))
              (anzu--overflow-p
               (format " %s+ " total))
              (t
               (format " %s/%d " here total))))
      'face (if (active) 'doom-modeline-panel)))))

(defun fancy-evil-s ()
  (format-mode-line
   (when (and evil-mode
              (or (assq 'evil-ex-substitute evil-ex-active-highlights-alist)
                  (assq 'evil-ex-global-match evil-ex-active-highlights-alist)
                  (assq 'evil-ex-buffer-match evil-ex-active-highlights-alist)))
     (propertize
      (let ((range (if evil-ex-range
                       (cons (car evil-ex-range) (cadr evil-ex-range))
                     (cons (line-beginning-position) (line-end-position))))
            (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
        (if pattern
            (format " %s matches " (how-many pattern (car range) (cdr range)))
          " - "))
      'face (if (active) 'doom-modeline-panel)))))

(defun doom-themes--overlay-sort (a b)
  (< (overlay-start a) (overlay-start b)))

(defun fancy-iedit ()
  (format-mode-line
   (when (and iedit-mode iedit-occurrences-overlays)
     (propertize
      (let ((this-oc (or (let ((inhibit-message t))
                           (iedit-find-current-occurrence-overlay))
                         (progn (iedit-prev-occurrence)
                                (iedit-find-current-occurrence-overlay))))
            (length (length iedit-occurrences-overlays)))
        (format " %s/%d "
                (if this-oc
                    (- length
                       (length (memq this-oc (sort (append iedit-occurrences-overlays nil)
                                                   #'doom-themes--overlay-sort)))
                       -1)
                  "-")
                length))
      'face (if (active) 'doom-modeline-panel)))))

(setq-default mode-line-format
              '((:eval (yq/fancy-mode-line-render
                        (string-join (list evil-mode-line-tag (fancy-buffer-info)))
                        (string-join (list (fancy-center)))
                        (string-join (list (fancy-anzu) (fancy-macro-recoding) (fancy-iedit) (fancy-evil-s) (fancy-flycheck) (fancy-vcs) " " (fancy-major-mode)))
                        0
                        1))))