(define-module (forgoty packages retro-gaming)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system node)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages emulators)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl))

(define-public pegasus-frontend
  (package
    (name "pegasus-frontend")
    (version "weekly_2024w38")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mmatyas/pegasus-frontend")
             (commit version)
             (recursive? #t)))
       (sha256
        (base32 "04773v3b50wya7w3hgy6j8sli07aimsfc8njqsycg9gbqygm183l"))
       (file-name (git-file-name name version))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase-5
      #:tests? #f
      #:make-flags
      #~(list (string-append "INSTALL_DESKTOPDIR="
                             #$output "/share/applications")
              (string-append "INSTALL_ICONDIR="
                             #$output "/share/icons/hicolor"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'qt-wrap 'gst-wrap
            (lambda _
              (wrap-program (string-append #$output "/bin/pegasus-fe")
                `("GST_PLUGIN_PATH" suffix
                  (,(string-append #$gst-plugins-base "/lib/gstreamer-1.0")
                   ,(string-append #$gst-plugins-good "/lib/gstreamer-1.0")
                   ,(string-append #$gst-plugins-bad "/lib/gstreamer-1.0")))))))))
    (inputs (list sdl2
                  qtbase-5
                  qtsvg-5
                  qtdeclarative-5
                  qtgraphicaleffects
                  qtmultimedia-5
                  qtgamepad-5
                  gst-plugins-base
                  gst-plugins-bad
                  gst-plugins-good))
    (native-inputs (list qttools-5))
    (synopsis
     "A cross platform, customizable graphical frontend for launching emulators and managing your game collection.")
    (description
     "Pegasus is a graphical frontend for browsing your game library and launching all kinds of emulators from the same place. It's focusing on customizability, cross platform support (including embedded) and high performance.")
    (home-page "https://pegasus-frontend.org")
    (license license:gpl3)))

(define-public skyscraper
  (package
    (name "skyscraper")
    (version "3.18.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Gemba/skyscraper")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13rykvb5byzg7l5khmryqg4qvax4byz81c86cd9qif4x35lkm2nx"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:make-flags
      #~(list (string-append "PREFIX="
                             #$output)
              (string-append "SYSCONFDIR="
                             #$output "/etc"))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)

          (add-after 'unpack 'enable-xdg
            (lambda _
              (substitute* "skyscraper.pro"
                (("#DEFINES\\+=XDG")
                 "DEFINES+=XDG"))))

          (add-before 'build 'qmake
            (lambda _
              (setenv "PREFIX"
                      #$output)
              (setenv "SYSCONFDIR"
                      (string-append #$output "/etc"))
              (invoke "qmake" "skyscraper.pro")))

          (add-after 'install 'fix-files
            (lambda _
              (wrap-program (string-append #$output "/bin/Skyscraper")
                `("PATH" suffix
                  (,(string-append #$7zip "/bin"))))
              (for-each (lambda (file)
                          (chmod file #o555))
                        (find-files (string-append #$output "/bin") "\\.py$"))
              (substitute* (string-append #$output "/bin/mdb2sqlite.sh")
                (("^")
                 (string-append "export PATH="
                                #$mdbtools "/bin:"
                                #$sqlite "/bin:$PATH\n"))))))))

    (inputs (list qtbase
                  qtimageformats
                  sqlite
                  mdbtools
                  7zip
                  python))
    (native-inputs (list qttools))
    (synopsis "Powerful and versatile game data scraper")
    (description
     "Skyscraper is a powerful and versatile game data scraper written in Qt and C++.")
    (home-page "https://gemba.github.io/skyscraper/")
    (license license:gpl3+)))
