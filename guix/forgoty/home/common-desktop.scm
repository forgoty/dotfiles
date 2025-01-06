(define-module (forgoty home common-desktop)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services dotfiles)
  #:use-module (guix gexp)
  #:use-module (forgoty home services desktop)
  #:use-module (forgoty home services emacs)
  #:export (common-desktop-home-services))

(define common-desktop-home-services
  (list
   ;; Concatenate addition profile that needed for dwmblocks running properly
   (simple-service 'custom/home-shell-profile-service-type
                   home-shell-profile-service-type
                   `(,(local-file "../../../dotfiles/.config/shell/profile")))

   ;; Pipewire
   (service home-pipewire-service-type)

   ;; Run user dbus session
   (service home-dbus-service-type)

   ;; Zsh
   (service home-zsh-service-type)

   ;; Dotfiles
   (service home-dotfiles-service-type
            (home-dotfiles-configuration (excluded '("README.md"
                                                     ".stow-local-ignore"))
                                         (directories '("../../../dotfiles"))))

   ;; Set up desktop environment
   (service home-desktop-service-type)

   ;; Emacs configuration
   (service home-emacs-config-service-type)))
