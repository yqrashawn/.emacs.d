(use-package plantuml-mode
  :straight t
  :mode ("\\\.puml\\'" "\\\.iuml\\'" "\\\.plantuml\\\'")
  :custom
  (plantuml-default-exec-mode 'executable)
  (org-plantuml-exec-mode 'plantuml)
  (plantuml-default-exec-mode 'server)
  (org-plantuml-executable-path "/usr/local/bin/plantuml")
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