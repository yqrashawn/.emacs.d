;; (eval-when-compile (require 'cl))

(setq-default js2-use-font-lock-faces t
              js2-mode-must-byte-compile nil
              js2-strict-trailing-comma-warning nil ; it's encouraged to use trailing comma in ES6
              js2-idle-timer-delay 0.5 ; NOT too big for real time syntax check
              js2-auto-indent-p nil
              js2-indent-on-enter-key nil ; annoying instead useful
              js2-skip-preprocessor-directives t
              js2-strict-inconsistent-return-warning nil ; return <=> return null
              js2-enter-indents-newline nil
              js2-bounce-indent-p t)

(setq javascript-common-imenu-regex-list
      '(("Attribute" " \\([a-z][a-zA-Z0-9-_]+\\) *= *\{[a-zA-Z0-9_.(), ]+\}\\( \\|$\\)" 1)
        ("Controller" "[. \t]controller([ \t]*['\"]\\([^'\"]+\\)" 1)
        ("Controller" "[. \t]controllerAs:[ \t]*['\"]\\([^'\"]+\\)" 1)
        ("Filter" "[. \t]filter([ \t]*['\"]\\([^'\"]+\\)" 1)
        ("State" "[. \t]state[(:][ \t]*['\"]\\([^'\"]+\\)" 1)
        ("Factory" "[. \t]factory([ \t]*['\"]\\([^'\"]+\\)" 1)
        ("Service" "[. \t]service([ \t]*['\"]\\([^'\"]+\\)" 1)
        ("Module" "[. \t]module( *['\"]\\([a-zA-Z0-9_.]+\\)['\"], *\\[" 1)
        ("ngRoute" "[. \t]when(\\(['\"][a-zA-Z0-9_\/]+['\"]\\)" 1)
        ("Directive" "[. \t]directive([ \t]*['\"]\\([^'\"]+\\)" 1)
        ("Event" "[. \t]\$on([ \t]*['\"]\\([^'\"]+\\)" 1)
        ("Config" "[. \t]config([ \t]*function *( *\\([^\)]+\\)" 1)
        ("Config" "[. \t]config([ \t]*\\[ *['\"]\\([^'\"]+\\)" 1)
        ("OnChange" "[ \t]*\$(['\"]\\([^'\"]*\\)['\"]).*\.change *( *function" 1)
        ("OnClick" "[ \t]*\$([ \t]*['\"]\\([^'\"]*\\)['\"]).*\.click *( *function" 1)
        ("Watch" "[. \t]\$watch( *['\"]\\([^'\"]+\\)" 1)
        ("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
        ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
        ;; {{ es6 beginning
        ("Function" "^[ \t]*\\([A-Za-z_$][A-Za-z0-9_$]+\\)[ \t]*([a-zA-Z0-9, ]*) *\{ *$" 1) ;; es6 fn1 () { }
        ("Function" "^[ \t]*\\([A-Za-z_$][A-Za-z0-9_$]+\\)[ \t]*=[ \t]*(?[a-zA-Z0-9, ]*)?[ \t]*=>" 1) ;; es6 fn1 = (e) =>
        ;; }}
        ("Task" "[. \t]task([ \t]*['\"]\\([^'\"]+\\)" 1)
        ))

;; js-mode imenu enhancement
;; @see http://stackoverflow.com/questions/20863386/idomenu-not-working-in-javascript-mode
(defun mo-js-imenu-make-index ()
  (save-excursion
    (imenu--generic-function javascript-common-imenu-regex-list)))

(defun mo-js-mode-hook ()
  (when (and (not (is-buffer-file-temp)) (not (derived-mode-p 'js2-mode)))
    (setq imenu-create-index-function 'mo-js-imenu-make-index)))

(add-hook 'js-mode-hook 'mo-js-mode-hook)
(eval-after-load 'js-mode
  '(progn
     ;; '$' is part of variable name like '$item'
     (modify-syntax-entry ?$ "w" js-mode-syntax-table)))

;; {{ patching imenu in js2-mode
(setq js2-imenu-extra-generic-expression javascript-common-imenu-regex-list)

(defvar js2-imenu-original-item-lines nil
  "List of line infomration of original imenu items.")

(defun js2-imenu--get-line-start-end (pos)
  (let* (b e)
    (save-excursion
      (goto-char pos)
      (setq b (line-beginning-position))
      (setq e (line-end-position)))
    (list b e)))

(defun js2-imenu--get-pos (item)
  (let* (val)
    (cond
     ((integerp item)
      (setq val item))

     ((markerp item)
      (setq val (marker-position item))))

    val))

(defun js2-imenu--get-extra-item-pos (item)
  (let* (val)
    (cond
     ((integerp item)
      (setq val item))

     ((markerp item)
      (setq val (marker-position item)))

     ;; plist
     ((and (listp item) (listp (cdr item)))
      (setq val (js2-imenu--get-extra-item-pos (cadr item))))

     ;; alist
     ((and (listp item) (not (listp (cdr item))))
      (setq val (js2-imenu--get-extra-item-pos (cdr item)))))

    val))

(defun js2-imenu--extract-line-info (item)
  "Recursively parse the original imenu items created by js2-mode.
The line numbers of items will be extracted."
  (let* (val)
    (if item
        (cond
         ;; Marker or line number
         ((setq val (js2-imenu--get-pos item))
          (push (js2-imenu--get-line-start-end val)
                js2-imenu-original-item-lines))

         ;; The item is Alist, example: (hello . 163)
         ((and (listp item) (not (listp (cdr item))))
          (setq val (js2-imenu--get-pos (cdr item)))
          (if val (push (js2-imenu--get-line-start-end val)
                        js2-imenu-original-item-lines)))

         ;; The item is a Plist
         ((and (listp item) (listp (cdr item)))
          (js2-imenu--extract-line-info (cadr item))
          (js2-imenu--extract-line-info (cdr item)))

         ;;Error handling
         (t (message "Impossible to here! item=%s" item))))))

(defun js2-imenu--item-exist (pos lines)
  "Try to detect does POS belong to some LINE"
  (let* (rlt)
    (dolist (line lines)
      (if (and (< pos (cadr line)) (>= pos (car line)))
          (setq rlt t)))
    rlt))

(defun js2-imenu--is-item-already-created (item)
  (unless (js2-imenu--item-exist
           (js2-imenu--get-extra-item-pos item)
           js2-imenu-original-item-lines)
    item))

(defun js2-imenu--check-single-item (r)
  (cond
   ((and (listp (cdr r)))
    (let (new-types)
      (setq new-types
            (delq nil (mapcar 'js2-imenu--is-item-already-created (cdr r))))
      (if new-types (setcdr r (delq nil new-types))
        (setq r nil))))
   (t (if (js2-imenu--item-exist (js2-imenu--get-extra-item-pos r)
                                 js2-imenu-original-item-lines)
          (setq r nil))))
  r)

(defun my-validate-json-or-js-expression (&optional not-json-p)
  "Validate buffer or select region as JSON.
If NOT-JSON-P is not nil, validate as Javascript expression instead of JSON."
  (interactive "P")
  (let* ((json-exp (if (region-active-p) (my-selected-str)
                     (my-buffer-str)))
         (jsbuf-offet (if not-json-p 0 (length "var a=")))
         errs
         first-err
         (first-err-pos (if (region-active-p) (region-beginning) 0)))
    (unless not-json-p
      (setq json-exp (format "var a=%s;"  json-exp)))
    (with-temp-buffer
      (insert json-exp)
      (unless (featurep 'js2-mode)
        (require 'js2-mode))
      (js2-parse)
      (setq errs (js2-errors))
      (cond
       ((not errs)
        (message "NO error found. Good job!"))
       (t
        ;; yes, first error in buffer is the last element in errs
        (setq first-err (car (last errs)))
        (setq first-err-pos (+ first-err-pos (- (cadr first-err) jsbuf-offet)))
        (message "%d error(s), first at buffer position %d: %s"
                 (length errs)
                 first-err-pos
                 (js2-get-msg (caar first-err))))))
    (if first-err (goto-char first-err-pos))))

(defun my-print-json-path (&optional hardcoded-array-index)
  "Print the path to the JSON value under point, and save it in the kill ring.
If HARDCODED-ARRAY-INDEX provided, array index in JSON path is replaced with it."
  (interactive "P")
  (cond
   ((memq major-mode '(js2-mode))
    (js2-print-json-path hardcoded-array-index))
   (t
    (let* ((cur-pos (point))
           (str (my-buffer-str)))
      (when (string= "json" (file-name-extension buffer-file-name))
        (setq str (format "var a=%s;" str))
        (setq cur-pos (+ cur-pos (length "var a="))))
      (unless (featurep 'js2-mode)
        (require 'js2-mode))
      (with-temp-buffer
        (insert str)
        (js2-init-scanner)
        (js2-do-parse)
        (goto-char cur-pos)
        (js2-print-json-path))))))

(defun js2-imenu--remove-duplicate-items (extra-rlt)
  (delq nil (mapcar 'js2-imenu--check-single-item extra-rlt)))

(defun js2-imenu--merge-imenu-items (rlt extra-rlt)
  "RLT contains imenu items created from AST.
EXTRA-RLT contains items parsed with simple regex.
Merge RLT and EXTRA-RLT, items in RLT has *higher* priority."
  ;; Clear the lines.
  (set (make-variable-buffer-local 'js2-imenu-original-item-lines) nil)
  ;; Analyze the original imenu items created from AST,
  ;; I only care about line number.
  (dolist (item rlt)
    (js2-imenu--extract-line-info item))

  ;; @see https://gist.github.com/redguardtoo/558ea0133daa72010b73#file-hello-js
  ;; EXTRA-RLT sample:
  ;; ((function ("hello" . #<marker 63>) ("bye" . #<marker 128>))
  ;;  (controller ("MyController" . #<marker 128))
  ;;  (hellworld . #<marker 161>))
  (setq extra-rlt (js2-imenu--remove-duplicate-items extra-rlt))
  (append rlt extra-rlt))

(eval-after-load 'js2-mode
  '(progn
     (defadvice js2-mode-create-imenu-index (around my-js2-mode-create-imenu-index activate)
       (let (rlt extra-rlt)
         ad-do-it
         (setq extra-rlt
               (save-excursion
                 (imenu--generic-function js2-imenu-extra-generic-expression)))
         (setq ad-return-value (js2-imenu--merge-imenu-items ad-return-value extra-rlt))
         ad-return-value))))
;; }}

(add-to-list 'auto-mode-alist '("\\.babelrc\\'" . js2-mode))

(defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
  "Workaround sgml-mode and follow airbnb component style."
  (save-excursion
    (beginning-of-line)
    (if (looking-at-p "^ +\/?> *$")
        (delete-char sgml-basic-offset))))

(setq-default js2-additional-externs
              '("$"
                "$A" ; salesforce lightning component
                "$LightningApp" ; salesforce
                "AccessifyHTML5"
                "Blob"
                "FormData"
                "KeyEvent"
                "Raphael"
                "React"
                "URLSearchParams"
                "__dirname" ; Node
                "_content" ; Keysnail
                "after"
                "afterEach"
                "angular"
                "app"
                "assert"
                "assign"
                "before"
                "beforeEach"
                "browser"
                "by"
                "clearInterval"
                "clearTimeout"
                "command" ; Keysnail
                "content" ; Keysnail
                "define"
                "describe"
                "documentRef"
                "global"
                "Bimsop"
                "THREE"
                "display" ; Keysnail
                "element"
                "expect"
                "ext" ; Keysnail
                "fetch"
                "gBrowser" ; Keysnail
                "goDoCommand" ; Keysnail
                "hook" ; Keysnail
                "inject"
                "isDev"
                "it"
                "jQuery"
                "jasmine"
                "key" ; Keysnail
                "ko"
                "log"
                "module"
                "plugins" ; Keysnail
                "process"
                "require"
                "setInterval"
                "setTimeout"
                "shell" ; Keysnail
                "tileTabs" ; Firefox addon
                "util" ; Keysnail
                "utag"))