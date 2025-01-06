(define-module (forgoty home t495)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services dotfiles)
  #:use-module (guix gexp)
  #:use-module (forgoty home services desktop)
  #:use-module (forgoty home services emacs))

(home-environment
  (services
   (list (service home-emacs-config-service-type)
         (service home-desktop-service-type)
         (service home-zsh-service-type)
         (simple-service 'custom/home-shell-profile-service-type
                         home-shell-profile-service-type
                         `(,(local-file
                             "../../../dotfiles/.config/shell/profile")))
         (service home-dbus-service-type)
         (service home-pipewire-service-type)
         (service home-dotfiles-service-type
                  (home-dotfiles-configuration (excluded '("README.md"
                                                           ".stow-local-ignore"))
                                               (directories '("../../../dotfiles")))))))
