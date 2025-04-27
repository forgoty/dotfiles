(define-module (forgoty packages ocr)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public (make-tesseract-ocr-language-package lang checksum)
  (package
    (name (string-append "tesseract-ocr-" lang))
    (version "4.1.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "https://github.com/tesseract-ocr/tessdata/raw/4.00/"
            lang ".traineddata"))
      (file-name (string-append lang ".traineddata"))
      (sha256 (base32 checksum))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan `(("." "share/tessdata"))))
    (home-page "https://github.com/tesseract-ocr/tessdata")
    (synopsis "tesseract-ocr language data")
    (description
     "Trained models with fast variant of the best LSTM models + legacy models")
    (license license:asl2.0)))
