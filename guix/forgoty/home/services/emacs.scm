(define-module (forgoty home services emacs)
  #:use-module (gnu home)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages emacs-build)
  #:use-module (forgoty packages emacs)
  #:export (home-emacs-config-service-type))

(define (home-emacs-configuration config)
  (list emacs
        ;; Tools
        emacs-restart-emacs
        emacs-google-c-style
        emacs-copilot
        emacs-direnv

        ;; Search and Completion
        emacs-magit
        emacs-cape
        emacs-consult
        emacs-corfu
        emacs-corfu-terminal
        emacs-embark
        emacs-embark-consult
        emacs-marginalia
        emacs-orderless
        emacs-vertico

        ;; Evil
        emacs-evil
        emacs-evil-collection
        emacs-evil-nerd-commenter
        emacs-evil-anzu
        emacs-evil-surround
        emacs-evil-visualstar
        emacs-embrace
        emacs-evil-embrace
        emacs-evil-iedit-state
        emacs-evil-textobj-tree-sitter

        ;; Tree-sitter Grammars
        tree-sitter-bash
        tree-sitter-c
        tree-sitter-cpp
        tree-sitter-cmake
        tree-sitter-css
        tree-sitter-elisp
        tree-sitter-go
        tree-sitter-gomod
        tree-sitter-gosum
        tree-sitter-html
        tree-sitter-javascript
        tree-sitter-json
        tree-sitter-latex
        tree-sitter-make
        tree-sitter-markdown
        tree-sitter-python
        tree-sitter-toml
        tree-sitter-typescript
        tree-sitter-dockerfile
        tree-sitter-plantuml
        tree-sitter-proto
        tree-sitter-zig
        tree-sitter-yaml

        ;; Input
        emacs-general
        emacs-which-key

        ;; IDE
        emacs-aggressive-indent
        emacs-package-lint
        emacs-zig-mode
        emacs-ibuffer-project
        emacs-protobuf-ts-mode
        emacs-package-lint-flymake
        emacs-flymake-golangci

        ;; Org
        emacs-org-super-agenda
        emacs-org-node

        ;; UI
        emacs-nerd-icons
        emacs-golden-ratio
        emacs-elisp-demos
        emacs-helpful
        emacs-doom-themes
        emacs-doom-modeline
        emacs-popwin
        emacs-winum
        emacs-tabspaces

        ;; Writing
        emacs-markdown-mode
        emacs-pandoc-mode
        emacs-auctex))

(define (home-emacs-shepherd-service config)
  (list
   (shepherd-service
    (provision '(emacs))
    (modules '((shepherd support)))
    (documentation "Emacs daemon")
    (start #~(make-forkexec-constructor
              (list #$(file-append emacs "/bin/emacs") "--fg-daemon")
              #:log-file (string-append %user-log-dir "/emacs.log")))
    (stop #~(make-kill-destructor)))))

(define home-emacs-config-service-type
  (service-type (name 'home-emacs-config)
                (description "A service for configuring Emacs in guix home")
                (extensions (list (service-extension home-profile-service-type
                                                     home-emacs-configuration)
                                  (service-extension home-shepherd-service-type
                                                     home-emacs-shepherd-service)))
                (default-value #f)))
