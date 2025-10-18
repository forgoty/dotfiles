(define-module (forgoty packages python-xyz)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages image-viewers))

(define-public ueberzug-bin
  (package
    (inherit ueberzug)
    (build-system pyproject-build-system)
    (inputs (modify-inputs (package-inputs ueberzug)
              (append meson-python)))))
