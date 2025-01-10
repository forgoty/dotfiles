(define-module (forgoty home services emacs)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (forgoty packages emacs)
  #:export (home-emacs-config-service-type))

(define (home-emacs-configuration config)
  (list emacs
        ;; Tools
        emacs-flycheck
        emacs-restart-emacs
	emacs-github-com-forgoty-flycheck-golangci-lint
        emacs-google-c-style

        ;; Search and Completion
        emacs-magit
        emacs-cape
        emacs-consult
        emacs-corfu
        emacs-corfu-terminal
        emacs-embark
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

        ;; Input
        emacs-general
        emacs-which-key

        ;; IDE
        emacs-editorconfig
        emacs-aggressive-indent
        emacs-package-lint
        emacs-zig-mode

        ;; Org
        emacs-org-super-agenda

        ;; UI
        emacs-nerd-icons
        emacs-golden-ratio
        emacs-elisp-demos
        emacs-helpful
        emacs-doom-themes
        emacs-doom-modeline
        emacs-popwin

        ;; Writing
        emacs-markdown-mode
        emacs-pandoc-mode
        emacs-auctex))

(define home-emacs-config-service-type
  (service-type (name 'home-emacs-config)
                (description "A service for configuring Emacs in guix home")
                (extensions (list (service-extension home-profile-service-type
                                   home-emacs-configuration)))
                (default-value #f)))
