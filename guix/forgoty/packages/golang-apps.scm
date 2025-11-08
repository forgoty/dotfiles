(define-module (forgoty packages golang-apps)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-xyz))

(define-public wtp
  (package
    (name "wtp")
    (version "2.3.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/satococoa/wtp.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0268308j8y76d52zv37by1yzg6dpzb7racgzw5mkc6i6mcc428kc"))))
    (build-system go-build-system)
    (arguments
      (list
        #:tests? #f
        #:install-source? #f
        #:embed-files
        #~(list "bash_autocomplete"
                "zsh_autocomplete")
        #:import-path "github.com/satococoa/wtp/cmd/wtp"
        #:unpack-path "github.com/satococoa/wtp"))
    (inputs (list go-github-com-stretchr-testify
                  go-github-com-urfave-cli-v3
                  go-go-yaml-in-yaml-v3
                  go-golang-org-x-term
                  go-gopkg-in-yaml-v3))
    (home-page "https://github.com/satococoa/wtp")
    (synopsis "git worktree tool with automated setup, branch tracking, and smart navigation")
    (description
     "Worktree Plus (wtp) - a powerful Git worktree management tool that extends git's worktree
functionality with automated setup, branch tracking,
and project-specific hooks.")
    (license license:isc)))
