(define-module (forgoty packages golang-check)
  #:use-module (guix build-system copy)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download))

(define-public golangci-lint
  (package
    (name "golangci-lint")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/golangci/golangci-lint/releases/download/"
                           "v" version
                           "/golangci-lint-" version "-linux-amd64.tar.gz"))
       (sha256
        (base32 "035yqab2h93a7p7g5k0din0cmhjz09cy356kc8w5x25rwx7rjpxi"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("golangci-lint" "bin/golangci-lint"))))
    (home-page "https://github.com/golangci/golangci-lint")
    (synopsis "golangci-lint is a fast Go linters runner.")
    (description "It runs linters in parallel, uses caching, supports YAML configuration, integrates with all major IDEs, and includes over a hundred linters.")
    (license gpl3)))
