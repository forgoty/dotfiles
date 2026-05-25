(define-module (forgoty packages streaming)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix utils)
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
  #:use-module (guix-science-nonfree packages cuda)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (nonguix build-system binary)
  #:use-module (nongnu packages nvidia)
  #:use-module (forgoty packages streaming))

(define-public sunshine-nvfbc
  (package
    (inherit sunshine)
    (arguments
     (substitute-keyword-arguments (package-arguments sunshine)
       ((#:configure-flags flags)
        #~(append
           (delete "-DSUNSHINE_ENABLE_CUDA=OFF" #$flags)
           (list
            "-DSUNSHINE_ENABLE_CUDA=ON"
            "-DCUDA_FAIL_ON_MISSING=ON"
            (string-append "-DCMAKE_CUDA_COMPILER="
                           (assoc-ref %build-inputs "cuda-toolkit")
                           "/bin/nvcc"))))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'add-nvfbc-patch
              (lambda _
                (substitute* "src/platform/linux/cuda.cpp"
                  (("libnvidia-fbc\\.so" all)
                   (string-append #$nvda "/lib/" all)))))
            (add-after 'install 'wrap-sunshine
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (bin (string-append out "/bin/sunshine"))
                       ;; rough LD path for patched nvidia driver, must be installed into system configuration
                       (driver "/run/current-system/profile/lib"))
                  (wrap-program bin
                    `("LD_LIBRARY_PATH" ":" prefix
                      (,driver))))))))))
    (inputs
     (modify-inputs (package-inputs sunshine)
       (append nvda cuda-cudart cuda-13.0)))))
