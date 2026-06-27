(define-module (forgoty packages streaming)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages upnp)
  #:use-module (gnu packages node)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages video)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages compression)
  #:use-module ((guix licenses)
                #:prefix license:))

(define build-deps
  (package
    (name "build-deps")
    (version "2026.516.30821")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/LizardByte/build-deps/releases/download/v"
             version "/Linux-x86_64-ffmpeg.tar.gz"))
       (sha256
        (base32 "1v667cf34scrkh1gkzhvkhx8qdsq796npqiszzlfyyw6qby1j8y3"))))
    (build-system copy-build-system)
    (home-page "https://github.com/LizardByte/build-deps")
    (synopsis "Prebuilt dependencies for LizardByte projects")
    (description
     "This is a common set of pre-compiled dependencies for LizardByte/Sunshine.")
    (license license:expat)))

(define-public sunshine
  (package
    (name "sunshine")
    (version "2026.516.143833")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/LizardByte/Sunshine.git")
             (commit (string-append "v" version))
             (recursive? #t)))
       (sha256
        (base32 "1b87qnwmwycz5w9avg85jiwi346fk4yx6y6nfpwaqimm4lxs2ayz"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list "-Wno-dev"
              "-DBOOST_USE_STATIC=false"
              "-DSUNSHINE_ENABLE_CUDA=OFF"
              "-DBUILD_DOCS=false"
              "-DBUILD_TESTS=OFF"
              "-DNPM_OFFLINE=ON"
              "-DSUNSHINE_SYSTEM_WAYLAND_PROTOCOLS=ON"
              "-DGLAD_SKIP_PIP_INSTALL=ON"
              (string-append "-DFFMPEG_PREPARED_BINARIES="
                             (assoc-ref %build-inputs "build-deps"))
              (string-append "-DOPENSSL_ROOT_DIR="
                             (assoc-ref %build-inputs "openssl")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'modify-src
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "cmake/targets/common.cmake"
                (("[^\n]*\"\\$\\{NPM\\}\" ci [^\n]*\n")
                 ""))
              (substitute* "cmake/compile_definitions/linux.cmake"
                (("\\$\\{CMAKE_SOURCE_DIR\\}/third-party/wlr-protocols")
                 (string-append (assoc-ref inputs "wlr-protocols")
                                "/share/wlr-protocols")))
              (substitute* "cmake/packaging/linux.cmake"
                (("\\$\\{UDEV_RULES_INSTALL_DIR\\}")
                 (string-append #$output "/lib/udev/rules.d"))
                (("\\$\\{SYSTEMD_USER_UNIT_INSTALL_DIR\\}")
                 "${SUNSHINE_ASSETS_DIR}/systemd/user"))
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
          (add-before 'configure 'set-env
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "BRANCH"
                      (string-append "v"
                                     #$version))
              (setenv "BUILD_VERSION"
                      #$version)))
          (add-before 'build 'unpack-npm-cache
            (lambda* (#:key inputs #:allow-other-keys)
              (invoke "tar" "xzf"
                      #$(this-package-native-input "npm-offline-cache.tar.gz"))
              (copy-file "package-lock.json" "../source/package-lock.json")
              (setenv "NPM_CONFIG_CACHE"
                      (string-append (getcwd) "/offline-cache"))
              (with-directory-excursion "../source"
                (invoke "npm" "ci" "--offline")
                (for-each (lambda (bin)
                            (patch-shebang (string-append "node_modules/.bin/"
                                            (readlink bin))))
                          (find-files "node_modules/.bin" ".*"))))))))
    (inputs (list eudev
                  libappindicator
                  boost
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
                  wayland
                  libx11
                  libxtst
                  libxrandr
                  libxfixes
                  libxcb
                  nlohmann-json
                  mesa
                  glib
                  pipewire
                  vulkan-loader
                  avahi))
    (native-inputs (list pkg-config
                         tar
                         node
                         wayland
                         wayland-protocols
                         wlr-protocols
                         python
                         python-jinja2
                         python-setuptools
                         shaderc
                         build-deps
                         (origin
                           (method url-fetch)
                           (uri (string-append
                                 "https://github.com/forgoty/sunshine-web-ui-builder/releases/download/v"
                                 version "/npm-offline-cache.tar.gz"))
                           (file-name "npm-offline-cache.tar.gz")
                           (sha256 (base32
                                    "12g7dc9p407cdicp3zkb4s95jngf0jp4k2nkfvfd2kw6ynl2g7ln")))))
    (home-page "https://app.lizardbyte.dev/Sunshine/")
    (synopsis "Self-hosted game stream host for Moonlight")
    (description
     "Sunshine is a self-hosted game stream host for Moonlight.
Offering low latency, cloud gaming server capabilities with support for AMD,
Intel, and Nvidia GPUs for hardware encoding. Software encoding is also available.")
    (license license:gpl3)))
sunshine
