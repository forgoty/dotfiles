;;; Eglot

;; eglot configuration
(setq eglot-workspace-configuration
      '((:gopls .
                ((buildFlags . ["-tags=unit,integration,e2e,component"])
                 (gofumpt . t)))))

;; Shutdown server when last managed buffer is killed
(customize-set-variable 'eglot-autoshutdown t)

;; Tree-Sitter
;;; Turn on max hightlights
(setq treesit-font-lock-level 4)

;; use electric indent.
(add-hook 'prog-mode-hook #'electric-indent-mode)

;; turn on editorconfig if it is available
(when (require 'editorconfig nil :noerror)
  (add-hook 'prog-mode-hook #'editorconfig-mode))

;; Project related
(setq project-vc-extra-root-markers
      '("package.json" "requirements.txt" ".git" "build.zig" "go.mod" "go.work"))

;; PlantUML
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
(setq plantuml-executable-path "/usr/bin/plantuml")
(setq plantuml-default-exec-mode 'executable)
(setq plantuml-output-type 'png)

;; Dockerfile
(add-to-list 'auto-mode-alist '("\\dockerfile\\'" . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("\\.Dockerfile\\'" . dockerfile-ts-mode))

;; Yaml
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))

;; Proto
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-ts-mode))

;; Flymake
(defun toggle-flymake-diagnostics-buffer ()
  (interactive)
  (let ((window-buffer (get-buffer-window (flymake--diagnostics-buffer-name))))
    (if window-buffer
        (quit-window t window-buffer)
      (flymake-show-diagnostics-buffer))))

(require 'ide-mode-golang)
(require 'ide-mode-js-ts)
(require 'ide-mode-c)
(require 'ide-mode-zig)

(provide 'emacs-config-ide)
