(use-package plantuml-mode
  :straight t
  :mode ("\\\.puml\\'" "\\\.iuml\\'" "\\\.plantuml\\\'")
  :custom
  (plantuml-default-exec-mode 'executable)
  (org-plantuml-executable-path "/usr/local/bin/plantuml")
  (org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2020.20/libexec/plantuml.jar")
  :init
  (with-eval-after-load 'ob
    (add-hook 'org-babel-after-execute-hook
              (lambda ()
                (when org-inline-image-overlays
                  (org-redisplay-inline-images))))

    (add-to-list 'org-babel-load-languages '(plantuml . t))
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

    ;; TODO check documents for ob-plantuml, see if there's any other way to delete the image first
    (defun org-babel-execute:plantuml (body params)
      "Execute a block of plantuml code with org-babel.
This function is called by `org-babel-execute-src-block'."
      (let* ((out-file (or (cdr (assq :file params))
                           (error "PlantUML requires a \":file\" header argument")))
             (cmdline (cdr (assq :cmdline params)))
             (in-file (org-babel-temp-file "plantuml-"))
             (java (or (cdr (assq :java params)) ""))
             (full-body (org-babel-plantuml-make-body body params))
             (cmd (if (string= "" org-plantuml-jar-path)
                      (error "`org-plantuml-jar-path' is not set")
                    (concat "java " java " -jar "
                            (shell-quote-argument
                             (expand-file-name org-plantuml-jar-path))
                            (if (string= (file-name-extension out-file) "png")
                                " -tpng" "")
                            (if (string= (file-name-extension out-file) "svg")
                                " -tsvg" "")
                            (if (string= (file-name-extension out-file) "eps")
                                " -teps" "")
                            (if (string= (file-name-extension out-file) "pdf")
                                " -tpdf" "")
                            (if (string= (file-name-extension out-file) "tex")
                                " -tlatex" "")
                            (if (string= (file-name-extension out-file) "vdx")
                                " -tvdx" "")
                            (if (string= (file-name-extension out-file) "xmi")
                                " -txmi" "")
                            (if (string= (file-name-extension out-file) "scxml")
                                " -tscxml" "")
                            (if (string= (file-name-extension out-file) "html")
                                " -thtml" "")
                            (if (string= (file-name-extension out-file) "txt")
                                " -ttxt" "")
                            (if (string= (file-name-extension out-file) "utxt")
                                " -utxt" "")
                            " -p -overwrite" cmdline " < "
                            (org-babel-process-file-name in-file)
                            " > "
                            (org-babel-process-file-name out-file)))))
        (print full-body)
        (unless (file-exists-p org-plantuml-jar-path)
          (error "Could not find plantuml.jar at %s" org-plantuml-jar-path))
        (with-temp-file in-file (insert full-body))
        (delete-file out-file)
        (message "%s" cmd) (org-babel-eval cmd "")
        nil))))



(use-package flycheck-plantuml
  :straight t
  :after (flycheck plantuml-mode)
  :hook (plantuml-mode . flycheck-plantuml-setup))