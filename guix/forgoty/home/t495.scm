(define-module (forgoty home t495)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (forgoty home common-desktop)
  #:use-module (forgoty home services dotfiles))

(define %host "t495")

(home-environment
  (services
   (append (list (service home-forgoty-dotfiles-service-type
                          (home-forgoty-dotfiles-configuration (host %host))))
           common-desktop-home-services
           %base-home-services)))
