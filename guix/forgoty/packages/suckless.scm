(define-module (forgoty packages suckless)
  #:use-module (gnu packages suckless)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages autotools)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix git-download))

(define-public forgoty-st
  (package
    (inherit lukesmithxyz-st)
    (name "forgoty-st")
    (inputs (modify-inputs (package-inputs st)
              (replace "libxft" libxft-bgra)
              (prepend libxext harfbuzz)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/forgoty/st.git")
             (commit "master")))
       (sha256
        (base32 "1bzxm284r0k0b09k2bh89jm4h4b67b7mdr26ivgdii9g0p15lr8z"))))))

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
        (base32 "0cjiw7fad6amracsibsglc78ibpgjfj5rlvwqdj7ciksz5rd19xn"))))))

(define-public forgoty-dwmblocks
  (let ((revision "1")
        (commit "460ae791bbb009a827338a925bc454ad0e5a77af"))
    (package
      (name "forgoty-dwmblocks")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/forgoty/dwmblocks.git")
               (commit "master")))
         (sha256
          (base32 "0pbm7d2k3lk8js63wixa3xvwr2f494hjbwi361gsbncjqi30shpc"))))
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
