(define-module (forgoty home services dotfiles)
  #:use-module (guix records)
  #:use-module ((guix utils) #:select (current-source-directory))
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles)
  #:export (home-forgoty-dotfiles-service-type
            home-forgoty-dotfiles-configuration
            home-config))

(define %dotfiles-root
  (canonicalize-path
    (string-append (current-source-directory) "/../../../../dotfiles")))

(define-record-type* <home-forgoty-dotfiles-configuration>
  home-forgoty-dotfiles-configuration
  make-home-forgoty-dotfiles-configuration
  home-forgoty-dotfiles-configuration?
  (host home-forgoty-dotfiles-configuration-host))

;; Dotfiles
(define (home-forgoty-dotfiles-configuration-files config)
  (let* ((host (home-forgoty-dotfiles-configuration-host config))
         (host-dir (string-append %dotfiles-root "/" host)))
    ;; regular dotfiles inserted by stow-like algorithm
    (home-dotfiles-configuration->files
      (home-dotfiles-configuration
        (excluded '("README.md"
                    ".stow-local-ignore"
                    ".emacs.d"))
        (directories (list host-dir))))))

(define home-forgoty-dotfiles-service-type
  (service-type (name 'home-dotfiles)
                (description "Dotfiles configuration")
                (extensions
                 (list (service-extension home-files-service-type
                                          home-forgoty-dotfiles-configuration-files)))))
