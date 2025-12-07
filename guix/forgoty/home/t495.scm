(define-module (forgoty home t495)
  #:use-module (gnu home)
  #:use-module (forgoty home test-home)
  #:use-module (forgoty home common-desktop))

(home-environment
  (services (append common-desktop-home-services test-home-services)))
