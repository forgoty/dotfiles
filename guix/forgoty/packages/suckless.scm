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
    (source (local-file "./src/st" #:recursive? #t))
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
    (synopsis "Luke Smith's fork of st")
    (home-page "https://github.com/LukeSmithxyz/st")
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
    (source (local-file "./src/dwm" #:recursive? #t))))

(define-public forgoty-dwmblocks
  (package
    (name "forgoty-dwmblocks")
    (version "0.0.1")
    (source (local-file "./src/dwmblocks" #:recursive? #t))
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
    (description "Modular status bar for dwm")
    (home-page "https://github.com/torrinfail/dwmblocks")
    (license license:x11)
    (synopsis "forgoty's dwmblocks")))

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
