;;; Eglot

;; eglot configuration
(setq eglot-workspace-configuration
      '((:gopls .
                ((buildFlags . ["-tags=unit,integration,e2e"])
                 (gofumpt . t)))))

;; Glue between flycheck and eglot
(require 'flycheck-eglot)
(global-flycheck-eglot-mode 1)

;; Shutdown server when last managed buffer is killed
(customize-set-variable 'eglot-autoshutdown t)

;; Tree-Sitter confiugration
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (latex "https://github.com/latex-lsp/tree-sitter-latex" "master" "src")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
     (plantuml "https://github.com/lyndsysimon/tree-sitter-plantuml")
     (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
     (proto "https://github.com/mitchellh/tree-sitter-proto")
     (zig "https://github.com/maxxnino/tree-sitter-zig")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(defun custom/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	      (treesit-install-language-grammar lang)
	      (message "`%s' parser was installed." lang)
	      (sit-for 0.75))))

;; Turn on max hightlights
(setq treesit-font-lock-level 4)

;; use electric indent.
(add-hook 'prog-mode-hook #'electric-indent-mode)

;; turn on editorconfig if it is available
(when (require 'editorconfig nil :noerror)
  (add-hook 'prog-mode-hook #'editorconfig-mode))

;; Project related
(setq project-vc-extra-root-markers
      '("package.json" "requirements.txt" ".git" "build.zig"))


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

(require 'ide-mode-golang)
(require 'ide-mode-js-ts)
(require 'ide-mode-c)
(require 'ide-mode-zig)

(provide 'emacs-config-ide)
