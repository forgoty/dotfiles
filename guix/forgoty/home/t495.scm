(define-module (forgoty home t495)
  #:use-module (gnu home)
  #:use-module (forgoty home common-desktop))

(home-environment
  (services common-desktop-home-services))
