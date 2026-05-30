(define-module (forgoty home services dotfiles)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module ((guix utils) #:select (current-source-directory))
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services shells)
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
         (host-dir (string-append %dotfiles-root "/" host))
         (emacs-config (string-append host-dir "/.config/emacs")))
    (append
      ;; regular dotfiles inserted by stow-like algorithm
      (home-dotfiles-configuration->files
        (home-dotfiles-configuration
          (excluded '("README.md"
                      ".stow-local-ignore"
                      ".emacs.d"))
          (directories (list host-dir))))
      ;; emacs configuration: the files normally living under
      ;; .config/emacs are deployed under .emacs.d so Emacs picks them up.
      `((".emacs.d/modules" ,(local-file (string-append emacs-config "/modules") #:recursive? #t))
        (".emacs.d/early-init.el" ,(local-file (string-append emacs-config "/early-init.el")))
        (".emacs.d/emacs.png" ,(local-file (string-append emacs-config "/emacs.png")))
        (".emacs.d/pomodoro-eaten.wav" ,(local-file (string-append emacs-config "/pomodoro-eaten.wav")))
        (".emacs.d/custom.el" ,(local-file (string-append emacs-config "/custom.el")))
        (".emacs.d/init.el" ,(local-file (string-append emacs-config "/init.el")))))))

(define home-forgoty-dotfiles-service-type
  (service-type (name 'home-dotfiles)
                (description "Dotfiles configuration")
                (extensions
                 (list (service-extension home-files-service-type
                                          home-forgoty-dotfiles-configuration-files)))))
