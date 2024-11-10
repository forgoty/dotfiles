(define-module (forgoty packages st)
	       #:use-module (gnu packages suckless)
	       #:use-module (guix packages)
	       #:use-module (gnu packages gtk)
	       #:use-module (gnu packages xorg)
	       #:use-module (forgoty packages libxft-bgra)
	       #:use-module (guix git-download))

(define-public forgoty-st
(package
(inherit lukesmithxyz-st)
(name "forgoty-st")
(inputs (modify-inputs (package-inputs st)
	(replace "libxft" libxft-bgra)
	(prepend libxext harfbuzz)))
(source (origin
      (method git-fetch)
      (uri (git-reference
	     (url "https://github.com/forgoty/st.git")
	     (commit "master")))
      (sha256
	(base32 "1bzxm284r0k0b09k2bh89jm4h4b67b7mdr26ivgdii9g0p15lr8z"))))))
