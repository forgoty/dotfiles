(define-module (forgoty packages streaming)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system node)
  #:use-module (guix build-system gnu)
  #:use-module (guix build utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages upnp)
  #:use-module (gnu packages node)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages video)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages linux)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (nonguix build-system binary))

(define (version-with-underscores version)
  (string-map (lambda (x) (if (eq? x #\.) #\_ x)) version))

(define-public boost-1.87
  (package
    (inherit boost)
    (version "1.87.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archives.boost.io/release/"
                                  version "/source/boost_"
                                  (version-with-underscores version) ".tar.bz2"))
              (sha256
               (base32
                "12bxa96qym7g2552kghgllp3bd7zi8vzx4nn7r0lnkscrcjvwmxg"))))))

(define sunshine-ui
  (package
    (name "sunshine-ui")
    (version "2025.628.4510")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/LizardByte/Sunshine.git")
                    (commit (string-append "v" version))
                    (recursive? #t)))
	      (file-name (git-file-name name version))
              (sha256
		(base32 "11i9jrkws8pb2a0hnjh3kshvaj00m0bljlqinizb4qmqlsiqbmf4"))))
    (build-system binary-build-system)
    (arguments
      (list
	#:phases
	#~(modify-phases %standard-phases
	  (add-before 'install 'npm-install
	    (lambda* (#:key inputs #:allow-other-keys)
		   (let ((npm (string-append (assoc-ref inputs "node") "/bin/npm")))
		   (invoke npm "install" "--loglevel=verbose")))))))
    (inputs
      (list
	node
	python))
    (home-page "https://app.lizardbyte.dev/Sunshine/")
    (synopsis "Helper package to build ui assets for sunshine")
    (description "Helper package to build ui assets for sunshine")
    (license license:gpl3)))

(define-public sunshine
  (package
    (name "sunshine")
    (version "2025.628.4510")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/LizardByte/Sunshine.git")
                    (commit (string-append "v" version))
                    (recursive? #t)))
              (sha256
               (base32 "11i9jrkws8pb2a0hnjh3kshvaj00m0bljlqinizb4qmqlsiqbmf4"))))
    (build-system cmake-build-system)
    (outputs '("out"
               "debug"))
    (arguments
     (list #:tests? #f
	   #:configure-flags
	   #~(list
	       "-Wno-dev"
	       "-DBOOST_USE_STATIC=false"
	       "-DSUNSHINE_ENABLE_CUDA=OFF"
	       "-DBUILD_DOCS=false"
	       "-DBUILD_TESTS=OFF"
         "-DNPM_OFFLINE=ON"
	       "VERBOSE=1"
	       "-DCMAKE_VERBOSE_MAKEFILE=ON"
	       (string-append "-DBOOST_INCLUDEDIR="
                               #$(this-package-input "boost")
                               "/include/")
                (string-append "-DBOOST_LIBRARYDIR="
                               #$(this-package-input "boost")
                               "/lib/")
                (string-append "-DOPENSSL_ROOT_DIR=" (assoc-ref %build-inputs "openssl")))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'modify-src
                 (lambda _
                   (substitute* "cmake/packaging/linux.cmake"
                     (("\\$\\{UDEV_RULES_INSTALL_DIR\\}")
                      (string-append #$output "/lib/udev/rules.d"))
                     (("\\$\\{SYSTEMD_USER_UNIT_INSTALL_DIR\\}")
                      "${SUNSHINE_ASSETS_DIR}/systemd/user"))
                   (substitute* "cmake/dependencies/common.cmake"
                     (("list\\(APPEND FFMPEG_PLATFORM_LIBRARIES mfx\\)")
                      ""))
                   (substitute* "src/platform/linux/publish.cpp"
                                (("libavahi-(common|client)\\.so" all)
                                 (string-append #$avahi "/lib/" all)))
                   (substitute* "src/platform/linux/x11grab.cpp"
                     (("libXrandr\\.so" all)
                      (string-append #$libxrandr "/lib/" all))
                     (("libXfixes\\.so" all)
                      (string-append #$libxfixes "/lib/" all))
                     (("libX11\\.so" all)
                      (string-append #$libx11 "/lib/" all))
                     (("libxcb(-shm|)\\.so" all)
                      (string-append #$libxcb "/lib/" all)))))
               (add-before 'build 'unpack-npm-cache
                           (lambda* (#:key inputs #:allow-other-keys)
                             (let ((cache-tar (assoc-ref inputs "npm-offline-cache")))
                               (invoke "tar" "xzf" cache-tar)
                               (copy-file "package-lock.json" "../source/package-lock.json")
                               (setenv "npm_config_cache" (string-append (getcwd) "/offline-cache"))
                               (setenv "npm_config_offline" "true")
                               (setenv "npm_config_prefer_offline" "true")
                               (with-directory-excursion "../source"
                                (invoke "npm" "ci" "--offline")
                                (for-each (lambda (bin)
                                            (patch-shebang (string-append "node_modules/.bin/" (readlink bin))))
                                            (find-files "node_modules/.bin" ".*")))))))))
    (inputs
     (list
      eudev
      libappindicator
      boost-1.87
      libcap
      curl
      libdrm
      libevdev
      miniupnpc
      libnotify
      numactl
      opus
      pulseaudio
      openssl
      libva
      libvdpau
      wayland
      libx11
      libxtst
      libxrandr
      libxfixes
      libxcb
      node
      nlohmann-json
      nss-certs
      mesa
      avahi))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("tar" ,tar)
       ("npm-offline-cache" ,(origin
                               (method url-fetch)
                               (uri (string-append "https://github.com/forgoty/sunshine-web-ui-builder/releases/download/v" version "/npm-offline-cache.tar.gz"))
                               (file-name "npm-offline-cache.tar.gz")
                               (sha256 (base32
                                         "162cgchqj18xy2ln2zv0d9kqjl2fp3wqp1xbbdr174yk1l098rin"))))))
    (home-page "https://app.lizardbyte.dev/Sunshine/")
    (synopsis "Self-hosted game stream host for Moonlight")
    (description "Sunshine is a self-hosted game stream host for Moonlight. Offering low latency, cloud gaming server capabilities with support for AMD, Intel, and Nvidia GPUs for hardware encoding. Software encoding is also available.")
    (license license:gpl3)))
sunshine
