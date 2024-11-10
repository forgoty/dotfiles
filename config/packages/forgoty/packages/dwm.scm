(define-module (forgoty packages dwm)
	       #:use-module (gnu packages suckless)
	       #:use-module (guix packages)
	       #:use-module (gnu packages xorg)
	       #:use-module (forgoty packages libxft-bgra)
	       #:use-module (guix git-download))

(define-public forgoty-dwm
(package
  (inherit dwm)
  (name "forgoty-dwm")
  (inputs (modify-inputs (package-inputs dwm)
			 (replace "libxft" libxft-bgra)))
  (source (origin
	    (method git-fetch)
	    (uri (git-reference
		   (url "https://github.com/forgoty/dwm.git")
		   (commit "master")))
	    (sha256
	      (base32 "1w20gamq41x4k0bjaqz06z0yxw9pd99yrilz7x2x958z82xkqkva"))))))
