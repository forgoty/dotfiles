(define-module (forgoty packages python-xyz)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages python-xyz))

(define-public python-ueberzug-18.3.1
  (package
    (inherit python-ueberzug)
    (version "18.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ueberzug" version))
       (sha256
        (base32
         "1sc05s72gvglsxsw4p0z5h47ygygw5h226vnaakas4ihjq9kiffl"))))
    (build-system pyproject-build-system)
    (inputs (modify-inputs (package-inputs python-ueberzug)
              (append pkg-config libxres meson-python)))
    (arguments
     (list #:tests? #f))))
