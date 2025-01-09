;; Tools
(add-to-list 'package-selected-packages 'flycheck-google-cpplint)
(add-to-list 'package-selected-packages 'copilot)
(add-to-list 'package-selected-packages 'flycheck)
(add-to-list 'package-selected-packages 'flycheck-pos-tip)
(add-to-list 'package-selected-packages 'restart-emacs)
(add-to-list 'package-selected-packages 'magit)
(add-to-list 'load-path (expand-file-name "site-lisp/flycheck-golangci-lint" user-emacs-directory))
(add-to-list 'package-selected-packages 'flycheck-eglot)

;; Search and Completion
(add-to-list 'package-selected-packages 'cape)
(add-to-list 'package-selected-packages 'consult)
(add-to-list 'package-selected-packages 'corfu)
(add-to-list 'package-selected-packages 'corfu-terminal)
(add-to-list 'package-selected-packages 'embark)
(add-to-list 'package-selected-packages 'embark-consult)
(add-to-list 'package-selected-packages 'marginalia)
(add-to-list 'package-selected-packages 'orderless)
(add-to-list 'package-selected-packages 'vertico)
(add-to-list 'package-selected-packages 'vertico-directory)

;; Evil
(add-to-list 'package-selected-packages 'evil)
(add-to-list 'package-selected-packages 'evil-collection)
(add-to-list 'package-selected-packages 'evil-nerd-commenter)
(add-to-list 'package-selected-packages 'evil-anzu)
(add-to-list 'package-selected-packages 'evil-nerd-commentary)
(add-to-list 'package-selected-packages 'evil-embrace)
(add-to-list 'package-selected-packages 'evil-surround)
(add-to-list 'package-selected-packages 'evil-visualstar)
(add-to-list 'package-selected-packages 'evil-iedit-state)
(add-to-list 'package-selected-packages 'evil-textobj-tree-sitter)

;; Input
(add-to-list 'package-selected-packages 'general)
(add-to-list 'package-selected-packages 'which-key)
(add-to-list 'package-selected-packages 'winum)

;; IDE
(add-to-list 'package-selected-packages 'editorconfig)
(add-to-list 'package-selected-packages 'ibuffer-project)
(add-to-list 'package-selected-packages 'protobuf-ts-mode)
(add-to-list 'package-selected-packages 'aggressive-indent)
(add-to-list 'package-selected-packages 'package-lint)
(add-to-list 'package-selected-packages 'package-lint-flymake)
(add-to-list 'package-selected-packages 'zig-mode)

;; UI
(add-to-list 'package-selected-packages 'nerd-icons) ;; after install run M-x nerd-icons-install-fonts
(add-to-list 'package-selected-packages 'elisp-demos)
(add-to-list 'package-selected-packages 'helpful)
(add-to-list 'package-selected-packages 'doom-themes)
(add-to-list 'package-selected-packages 'doom-modeline)
(add-to-list 'package-selected-packages 'popwin)
(add-to-list 'package-selected-packages 'tabspaces)
(add-to-list 'package-selected-packages 'golden-ratio)

;; Writing
(add-to-list 'package-selected-packages 'markdown-mode)
(add-to-list 'package-selected-packages 'pandoc-mode)
(when (executable-find "latex")
  (add-to-list 'package-selected-packages 'auctex)
  (when (executable-find "latexmk")
    (add-to-list 'package-selected-packages 'auctex-latexmk)))

;; Org-Mode
(add-to-list 'package-selected-packages 'org-super-agenda)

(provide 'emacs-config-packages)
