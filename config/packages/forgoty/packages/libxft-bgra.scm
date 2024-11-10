(define-module (forgoty packages libxft-bgra)
	       #:use-module (guix packages)
	       #:use-module (gnu packages xorg)
	       #:use-module (gnu packages pkg-config)
	       #:use-module (gnu packages autotools)
	       #:use-module (guix git-download))

(define-public libxft-bgra
(package
(inherit libxft)
(name "libxft-bgra")
(native-inputs
  (list pkg-config autoconf automake libtool))
(source (origin
      (method git-fetch)
      (uri (git-reference
	     (url "https://github.com/uditkarode/libxft-bgra")
	     (commit "master")))
      (sha256
	(base32 "1pnz43kqrd2s6q0kgxjdz0ax7a6rf7jm635whnlgq7h6q8hmv15r"))))))
