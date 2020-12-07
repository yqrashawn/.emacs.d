(use-package plantuml-mode
  :straight t
  :mode ("\\\.puml\\'" "\\\.iuml\\'" "\\\.plantuml\\\'")
  :custom
  (plantuml-default-exec-mode 'executable)
  (org-plantuml-executable-path "/usr/local/bin/plantuml")
  (org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2020.22/libexec/plantuml.jar")
  :init
  (with-eval-after-load 'ob
    (add-hook 'org-babel-after-execute-hook
              (lambda ()
                (when org-inline-image-overlays
                  (org-redisplay-inline-images))))

    (add-to-list 'org-babel-load-languages '(plantuml . t))
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))))



(use-package flycheck-plantuml
  :straight t
  :after (flycheck plantuml-mode)
  :hook (plantuml-mode . flycheck-plantuml-setup))