(define-module (forgoty packages emacs)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix git-download))

(define-public emacs-github-com-forgoty-flycheck-golangci-lint
  (let ((commit "4277523d57469c3a686e5a0403a49cdb11c52a17")
        (revision "0"))
    (package
      (name "emacs-github-com-forgoty-flycheck-golangci-lint")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/forgoty/flycheck-golangci-lint")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "01aqsdzwkczspr12qlfbrcgfpq10n7930y8ipg05ik3vzcsm2zzj"))))
      (build-system emacs-build-system)
      (propagated-inputs (list emacs-flycheck))
      (home-page "https://github.com/forgoty/flycheck-golangci-lint")
      (synopsis "flycheck integration with golangci-lint")
      (description
       "Fork of https://github.com/weijiangan/flycheck-golangci-lint with some improvements")
      (license license:gpl3+))))
