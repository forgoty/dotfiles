(define-module (forgoty packages shellutils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system copy))

(define-public forgoty-shellutils
  (package
    (name "forgoty-shellutils")
    (version "0.1.0")
    (source (local-file "./src/shellutils" #:recursive? #t))
    (build-system copy-build-system)
    (arguments
      `(#:install-plan `(("." "bin/"))))
    (home-page "https://github.com/satococoa/wtp")
    (synopsis "Collection of shell utilities")
    (description "Collection of shell utilities")
    (license license:gpl3+)))
