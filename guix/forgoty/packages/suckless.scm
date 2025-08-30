(define-module (forgoty packages suckless)
  #:use-module (gnu packages suckless)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages autotools)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix git-download))

(define-public forgoty-st
  (package
    (name "forgoty-st")
    (version "0.8.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/forgoty/st.git")
             (commit "master")))
       (sha256
        (base32 "0di01khw1xlg277znfchzxq9q8lqx59yl167kz9qf77wvcwh4i9r"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:make-flags
      #~(list
         (string-append "CC=" #$(cc-for-target))
         (string-append "TERMINFO=" #$output "/share/terminfo")
         (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure))))
    (inputs
     (list libx11
           libxft
           fontconfig
           libxft-bgra
           libxext
           harfbuzz
           freetype))
    (native-inputs
     (list ncurses
           pkg-config))
    (home-page "https://github.com/LukeSmithxyz/st")
    (synopsis "Luke Smith's fork of st")
    (description
     "This package is Luke's fork of the suckless simple terminal (st) with
Vimbndings and Xresource compatibility.")
    (license license:expat)))

(define-public luke-dmenu
  (package
    (inherit dmenu)
    (name "luke-dmenu")
    (inputs (modify-inputs (package-inputs dmenu)
              (replace "libxft" libxft-bgra)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/LukeSmithxyz/dmenu.git")
             (commit "master")))
       (sha256
        (base32 "1cr0rjcs57ml8qwqizl7aykx03al2q1ng1x3nvnkif3lr07yrja1"))))))

(define-public forgoty-dwm
  (package
    (inherit dwm)
    (name "forgoty-dwm")
    (inputs (modify-inputs (package-inputs dwm)
              (replace "libxft" libxft-bgra)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/forgoty/dwm.git")
             (commit "master")))
       (sha256
        (base32 "1bv3qh6mmfcfi152q6bkcayn07nmql8rws6ig5m4knc4x0fdhfyq"))))))

(define-public forgoty-dwmblocks
  (let ((revision "1")
        (commit "6801242d65473949f863caba091b73ab968b8c9f"))
    (package
      (name "forgoty-dwmblocks")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/forgoty/dwmblocks.git")
               (commit "master")))
         (sha256
          (base32 "0hk2rs05cf239znwhmmfmp2rq4v21z0ayfg26dgdx4zzvvln6h1c"))))
      (version (git-version "0" revision commit))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ;no check target
         #:make-flags (list (string-append "CC="
                                           ,(cc-for-target))
                            (string-append "PREFIX=" %output)
                            (string-append "FREETYPEINC="
                                           (assoc-ref %build-inputs "freetype")
                                           "/include/freetype2"))
         #:phases (modify-phases %standard-phases
                    (delete 'configure) ;no configure script
		    (add-after 'install 'install-scripts
			       (lambda* (#:key outputs #:allow-other-keys)
					(let* ((out (assoc-ref outputs "out"))
					       (bin (string-append out "/bin")))
					  (mkdir-p bin)
					  (copy-recursively "scripts" bin)
					  #t))))))
      (inputs (list freetype libxft-bgra libx11 libxinerama))
      (home-page "https://github.com/forgoty/dwmblocks.git")
      (description "forgoty's dwmblocks")
      (license license:x11)
      (synopsis "forgoty's dwmblocks"))))

(define-public libxft-bgra
  (package
    (inherit libxft)
    (name "libxft-bgra")
    (native-inputs (list pkg-config autoconf automake libtool))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uditkarode/libxft-bgra")
             (commit "master")))
       (sha256
        (base32 "1pnz43kqrd2s6q0kgxjdz0ax7a6rf7jm635whnlgq7h6q8hmv15r"))))))
