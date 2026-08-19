(define-module (forgoty packages coding-agents)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages compression) #:select (zlib unzip))
  #:use-module ((gnu packages base) #:select (glibc))
  #:use-module ((nonguix build-system binary) #:select (binary-build-system)))

(define-public eca
  (package
   (name "eca")
   (version "0.154.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://github.com/editor-code-assistant/eca/releases/download/"
           version "/eca-native-linux-amd64.zip"))
     (sha256
      (base32 "1a883z5bmsxm7ngygliyd4xrnhql6bj9w19rh7wq38wv9rhjvlzi"))))
   (build-system binary-build-system)
   (native-inputs (list unzip))
   (inputs (list glibc zlib))
   (arguments
    (list
     #:install-plan #~'(("eca" "bin/"))
     #:patchelf-plan #~'(("eca" ("glibc" "zlib")))
     #:phases
     #~(modify-phases %standard-phases
         (add-after 'unpack 'chmod
           (lambda _ (chmod "eca" #o755))))))
   (home-page "https://github.com/editor-code-assistant/eca")
   (synopsis "AI pair programming tool agnostic of editor")
   (description
    "ECA (Editor Code Assistant) is an AI pair programming tool that provides
AI-assisted coding capabilities independent of any specific editor.  It works
across different editors and development environments.")
   (license license:asl2.0)))

