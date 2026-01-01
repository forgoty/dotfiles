(define-module (forgoty home services dotfiles)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services shells)
  #:export (home-forgoty-dotfiles-service-type home-config))

;; Dotfiles
(define (home-forgoty-dotfiles-configuration config)
  (append
    ;; regular dotfiles inserted by stow-like algorithm
    (home-dotfiles-configuration->files (home-dotfiles-configuration
					(excluded '("README.md"
						    ".stow-local-ignore"
						    ".emacs.d"))
					(directories '("../../../../dotfiles"))))
    ;; emacs configuration
    `((".emacs.d/modules" ,(local-file "../../../../dotfiles/.config/emacs/modules" #:recursive? #t))
    (".emacs.d/early-init.el" ,(local-file "../../../../dotfiles/.config/emacs/early-init.el"))
    (".emacs.d/emacs.png" ,(local-file "../../../../dotfiles/.config/emacs/emacs.png"))
    (".emacs.d/custom.el" ,(local-file "../../../../dotfiles/.config/emacs/custom.el"))
    (".emacs.d/init.el" ,(local-file "../../../../dotfiles/.config/emacs/init.el")))))

(define home-forgoty-dotfiles-service-type
  (service-type (name 'home-dotfiles)
                (description "Dotfiles configuration")
                (extensions (list (service-extension home-files-service-type
						     home-forgoty-dotfiles-configuration)))
                (default-value #f)))
