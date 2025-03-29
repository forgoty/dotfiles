(define-module (forgoty packages version-control)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (gnu packages version-control)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:export (git-sync))

(define-public git-sync
  (let ((commit "7242291edf543ecc1bb9de8f47086bb69a5cb9f7")
        (checksum "1sfwdi1zmg9i40026qcjr5vr10ha958pqxa35g668bl4ky15alxp")
        (revision "0"))
    (package
      (name "git-sync")
      (version (git-version "0" revision commit))
      (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/simonthum/git-sync/")
                      (commit commit)))
              (file-name (git-file-name name version))
              (sha256
                (base32 checksum))))
      (build-system copy-build-system)
      (arguments
      `(#:install-plan `(("git-sync" "bin/git-sync"))))
      (synopsis "Synchronize tracking repositories")
      (description "This scrips intends to sync near-automatically via git in \"tracking\" repositories where a nice history is not as crucial
  as having one.")
      (home-page "https://github.com/simonthum/git-sync/")
      (propagated-inputs (list git))
      (license cc0))))
