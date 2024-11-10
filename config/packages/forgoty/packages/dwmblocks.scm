(define-module (forgoty packages dwmblocks)
	       #:use-module (guix packages)
	       #:use-module (guix utils)
	       #:use-module (guix build-system gnu)
	       #:use-module (gnu packages fontutils)
	       #:use-module (gnu packages xorg)
	       #:use-module (forgoty packages libxft-bgra)
	       #:use-module ((guix licenses) #:prefix license:)
	       #:use-module (guix git-download))

(define-public forgoty-dwmblocks
	       (let ((revision "1")
		     (commit "1c9744ac7ded4fff8171169bc0b9736f3acd4cfe"))
		 (package
		   (name "forgoty-dwmblocks")
		   (source
		     (origin
		       (method git-fetch)
		       (uri (git-reference
			      (url "https://github.com/forgoty/dwmblocks.git")
			      (commit "master")))
		       (sha256
			 (base32 "0zj2avxzr6bj8pnclh3vpxvpng3fcn4p0rd8fx8pw3gdzf2q47sn"))))
		   (version (git-version "0" revision commit))
		   (build-system gnu-build-system)
		   (arguments
		     `(#:tests? #f                      ; no check target
		       #:make-flags
		       (list
			 (string-append "CC=" ,(cc-for-target))
			 (string-append "PREFIX=" %output)
			 (string-append "FREETYPEINC="
					(assoc-ref %build-inputs "freetype")
					"/include/freetype2"))
		       #:phases
		       (modify-phases %standard-phases
				      (delete 'configure))))         ; no configure script
		   (inputs
		     (list freetype libxft-bgra libx11 libxinerama))
		   (home-page "https://github.com/LukeSmithxyz/dwmblocks.git")
		   (description "forgoty's dwmblocks")
		   (license license:x11)
		   (synopsis "forgoty's dwmblocks"))))
