(define-module (forgoty packages dmenu)
	       #:use-module (gnu packages suckless)
	       #:use-module (guix packages)
	       #:use-module (gnu packages xorg)
	       #:use-module (forgoty packages libxft-bgra)
	       #:use-module (guix git-download))

(define-public luke-dmenu
(package
  (inherit dmenu)
  (name "luke-dmenu")
  (inputs (modify-inputs (package-inputs dmenu)
			 (replace "libxft" libxft-bgra)))
  (source (origin
	    (method git-fetch)
	    (uri (git-reference
		   (url "https://github.com/LukeSmithxyz/dmenu.git")
		   (commit "master")))
	    (sha256
	      (base32 "1cr0rjcs57ml8qwqizl7aykx03al2q1ng1x3nvnkif3lr07yrja1"))))))
