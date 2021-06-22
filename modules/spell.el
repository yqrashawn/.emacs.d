;;; spell.el --- spell check config from doom emacs  -*- lexical-binding: t; -*-

;; Copyright © 2021, Rashawn Zhang, all rights reserved.

;; Author: Rashawn Zhang <namy.19@gmail.com>
;; Created: 20 June 2021
;; Keywords: spell, spell-fu

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;  spell check config from doom emacs

;;; Code:

(defun +spell--correct (replace poss word orig-pt start end)
  (cond ((eq replace 'ignore)
         (goto-char orig-pt)
         nil)
        ((eq replace 'save)
         (goto-char orig-pt)
         (ispell-send-string (concat "*" word "\n"))
         (ispell-send-string "#\n")
         (setq ispell-pdict-modified-p '(t)))
        ((or (eq replace 'buffer) (eq replace 'session))
         (ispell-send-string (concat "@" word "\n"))
         (add-to-list 'ispell-buffer-session-localwords word)
         (or ispell-buffer-local-name ; session localwords might conflict
             (setq ispell-buffer-local-name (buffer-name)))
         (if (null ispell-pdict-modified-p)
             (setq ispell-pdict-modified-p
                   (list ispell-pdict-modified-p)))
         (goto-char orig-pt)
         (if (eq replace 'buffer)
             (ispell-add-per-file-word-list word)))
        (replace
         (let ((new-word (if (atom replace)
                             replace
                           (car replace)))
               (orig-pt (+ (- (length word) (- end start))
                           orig-pt)))
           (unless (equal new-word (car poss))
             (delete-region start end)
             (goto-char start)
             (insert new-word))))
        ((goto-char orig-pt)
         nil)))

(defun +spell-correct-ivy-fn (candidates word)
  (ivy-read (format "Corrections for %S: " word) candidates))

(defun +spell-correct-generic-fn (candidates word)
  (completing-read (format "Corrections for %S: " word) candidates))

;;;###autoload
(defun +spell/correct ()
  "Correct spelling of word at point."
  (interactive)
  ;; spell-fu fails to initialize correctly if it can't find aspell or a similar
  ;; program. We want to signal the error, not tell the user that every word is
  ;; spelled correctly.
  (unless (;; This is what spell-fu uses to check for the aspell executable
           or (and ispell-really-aspell ispell-program-name)
              (executable-find "aspell"))
    (user-error "Aspell is required for spell checking"))

  (ispell-set-spellchecker-params)
  (save-current-buffer
    (ispell-accept-buffer-local-defs))
  (cl-destructuring-bind (start . end)
      (or (bounds-of-thing-at-point 'word)
          (user-error "No word at point"))
    (let ((word (thing-at-point 'word t))
          (orig-pt (point))
          poss ispell-filter)
      (ispell-send-string "%\n")
      (ispell-send-string (concat "^" word "\n"))
      (while (progn (accept-process-output ispell-process)
                    (not (string= "" (car ispell-filter)))))
      ;; Remove leading empty element
      (setq ispell-filter (cdr ispell-filter))
      ;; ispell process should return something after word is sent. Tag word as
      ;; valid (i.e., skip) otherwise
      (unless ispell-filter
        (setq ispell-filter '(*)))
      (when (consp ispell-filter)
        (setq poss (ispell-parse-output (car ispell-filter))))
      (cond
       ((or (eq poss t) (stringp poss))
        ;; don't correct word
        (message "%s is correct" (funcall ispell-format-word-function word))
        t)
       ((null poss)
        ;; ispell error
        (error "Ispell: error in Ispell process"))
       (t
        ;; The word is incorrect, we have to propose a replacement.
        (setq res (funcall +spell-correct-interface (nth 2 poss) word))
        ;; Some interfaces actually eat 'C-g' so it's impossible to stop rapid
        ;; mode. So when interface returns nil we treat it as a stop.
        (unless res (setq res (cons 'break word)))
        (cond
         ((stringp res)
          (+spell--correct res poss word orig-pt start end))
         ((let ((cmd (car res))
                (wrd (cdr res)))
            (unless (or (eq cmd 'skip)
                        (eq cmd 'break)
                        (eq cmd 'stop))
              (+spell--correct cmd poss wrd orig-pt start end)
              (unless (string-equal wrd word)
                (+spell--correct wrd poss word orig-pt start end))))))
        (ispell-pdict-save t))))))

;;;###autoload
(defalias '+spell/add-word #'spell-fu-word-add)
;;;###autoload
(defalias '+spell/remove-word #'spell-fu-word-remove)
;;;###autoload
(defalias '+spell/next-error #'spell-fu-goto-next-error)
;;;###autoload
(defalias '+spell/previous-error #'spell-fu-goto-previous-error)

(use-feature ispell
  :defer t
  :commands (ispell-word ispell)
  :config
  ;; Don't spellcheck org blocks
  (pushnew! ispell-skip-region-alist
            '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
            '("#\\+BEGIN_SRC" . "#\\+END_SRC")
            '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

  ;; Enable either aspell, hunspell or enchant.
  ;;   If no module flags are given, enable either aspell, hunspell or enchant
  ;;     if their binary is found.
  ;;   If one of the flags `+aspell', `+hunspell' or `+enchant' is given,
  ;;     only enable that spell checker.
  (pcase (cond ((executable-find "hunspell")  'hunspell)
               ((executable-find "enchant-2") 'enchant))
    (`hunspell
     (setq ispell-program-name "hunspell"))

    (`enchant
     (setq ispell-program-name "enchant-2"))))

(use-package spell-fu
  :straight t
  ;; :hook ((text-mode yaml-mode conf-mode prog-mode) . spell-fu-mode)
  :hook ((text-mode git-commit-mode) . spell-fu-mode)
  :custom
  (spell-fu-idle-delay 2)
  (spell-fu-directory (concat spacemacs-cache-directory "spell-fu" ".emacs-spell-fu"))
  :preface
  (defvar +spell-correct-interface #'+spell-correct-ivy-fn)
  :init
  (defvar +spell-excluded-faces-alist
    '((markdown-mode
       . (markdown-code-face
          markdown-html-attr-name-face
          markdown-html-attr-value-face
          markdown-html-tag-name-face
          markdown-inline-code-face
          markdown-link-face
          markdown-markup-face
          markdown-plain-url-face
          markdown-reference-face
          markdown-url-face))
      (org-mode
       . (org-block
          org-block-begin-line
          org-block-end-line
          org-code
          org-date
          org-formula
          org-latex-and-related
          org-link
          org-meta-line
          org-property-value
          org-ref-cite-face
          org-special-keyword
          org-tag
          org-todo
          org-todo-keyword-done
          org-todo-keyword-habt
          org-todo-keyword-kill
          org-todo-keyword-outd
          org-todo-keyword-todo
          org-todo-keyword-wait
          org-verbatim))
      (latex-mode
       . (font-latex-math-face
          font-latex-sedate-face
          font-lock-function-name-face
          font-lock-keyword-face
          font-lock-variable-name-face)))
    "Faces in certain major modes that spell-fu will not spellcheck.")
  (global-set-key [remap ispell-word] #'+spell/correct)
  (setq ispell-personal-dictionary (concat user-emacs-directory "ispell-personal-dictionary"))
  :config
  (defadvice! +spell--fix-face-detection-a (orig-fn &rest args)
    "`spell-fu--faces-at-point' uses face detection that won't penetrary
overlays (like `hl-line'). This makes `spell-fu-faces-exclude' demonstrably less
useful when it'll still spellcheck excluded faces on any line that `hl-line' is
displayed on, even momentarily."
    :around #'spell-fu--faces-at-point
    (letf! (defun get-char-property (pos prop &optional obj)
             (or (plist-get (text-properties-at pos) prop)
                 (funcall get-char-property pos prop obj)))
           (apply orig-fn args)))

  (defadvice! +spell--create-word-dict-a (_word words-file _action)
    "Prevent `spell-fu--word-add-or-remove' from throwing non-existant
directory errors when writing a personal dictionary file (by creating the
directory first)."
    :before #'spell-fu--word-add-or-remove
    (unless (file-exists-p words-file)
      (make-directory (file-name-directory words-file) t)
      (with-temp-file words-file
        (insert (format "personal_ws-1.1 %s 0\n" ispell-dictionary)))))

  (add-hook! 'spell-fu-mode-hook
             (defun +spell-init-excluded-faces-h ()
               "Set `spell-fu-faces-exclude' according to `+spell-excluded-faces-alist'."
               (when-let (excluded (cdr (cl-find-if #'derived-mode-p +spell-excluded-faces-alist :key #'car)))
                 (setq-local spell-fu-faces-exclude excluded)))))

(provide 'spell)
;;; spell.el ends here