(define-module (forgoty systems guldan)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system image)
  #:use-module (gnu services base)
  #:use-module (gnu services guix)
  #:use-module (gnu packages linux)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (nongnu services nvidia)
  #:use-module (forgoty services sunshine)
  #:use-module (forgoty home guldan)
  #:use-module (forgoty systems base-system))

(use-service-modules desktop
                     linux
                     xorg
                     audio
                     ssh
                     virtualization
                     containers
                     networking)

(use-package-modules admin
                     fonts
                     gnome
                     linux
                     audio
                     file-systems
                     vim
                     nfs
                     shells
                     video
                     xorg
                     package-management)
(define evdi-linux
  (package
    (inherit evdi)
    (name "evdi")
    (version "1.14.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/DisplayLink/evdi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mg9gbzgxwgdcniy9kijsyj0asvmdxd63kh88810igzsxs3185jb"))))
    (arguments
     (list #:tests? #f
           #:linux linux
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'chdir
                          (lambda _
                            (chdir "module")))
                        (add-after 'unpack 'fix-os-release
                          (lambda _
                            (define (touch file)
                              (call-with-output-file file
                                (const #t)))
                            (let* ((hard-path "/etc/os-release")
                                    (fixed-path (string-append #$output hard-path)))
                              ;; Make it relative
                              ;; Update hardcoded path to something
                              ;; within the build enviroment.
                              (substitute* "module/Makefile"
                                ((hard-path)
                                  fixed-path))
                              ;; Create directory for the dummy file.
                              (mkdir-p (string-append #$output "/etc"))
                              (touch fixed-path)))))))))

(define system-packages
  (list bluez
        bluez-alsa
        ;; evdi-linux
        fuse-exfat
        ntfs-3g
        xf86-video-dummy
        libva
        libva-utils
        neovim))

(define system-services
  (list (service openssh-service-type
                 (openssh-configuration (port-number 2222)))
        (service bluetooth-service-type
                 (bluetooth-configuration (auto-enable? #t)))
        (service sunshine-service-type)
        (service iptables-service-type)
        (service nvidia-service-type)
        (service guix-home-service-type `((%default-username ,guldan-home)))
        ;; (service kernel-module-loader-service-type '("evdi"))
        ;; (simple-service 'evdi-config etc-service-type
        ;;                 (list `("modprobe.d/evdi.conf"
        ;;                         ,(plain-file "evdi.conf"
        ;;                                      "options evdi initial_device_count=1"))))
        (service rootless-podman-service-type
          (rootless-podman-configuration
            (podman #f)
            (subgids
              (list (subid-range (name %default-username))))
            (subuids
              (list (subid-range (name %default-username))))))))

(define-public guldan
  (operating-system
    (locale "en_US.utf8")
    (timezone "Europe/Warsaw")
    (host-name "guldan")
    (kernel linux)
    (initrd microcode-initrd)
    (firmware (list linux-firmware))
    ;; (kernel-loadable-modules (list evdi-linux))
    (users (append
            (list (user-account
              (name %default-username)
              (comment (string-capitalize %default-username))
              (password (crypt "password" "$6$abc"))
              (group "users")
              (home-directory (string-append "/home/" %default-username))
              (shell (file-append zsh "/bin/zsh"))
              (supplementary-groups '("wheel"
                                      "netdev"
                                      "audio"
                                      "input"
                                      "tty"
                                      "video"
                                      "lp"
                                      "cgroup"
                                      "kvm"
                                      "libvirt"))))
            %base-user-accounts))
    (packages (append system-packages %base-packages))
    (services
      (append system-services
              (modify-services %desktop-services
                (delete gdm-service-type)
                (mingetty-service-type config =>
                  (mingetty-configuration
                    (inherit config)
                    (auto-login %default-username)))
                (guix-service-type config =>
                                  (guix-configuration (inherit config)
                                                      (substitute-urls (append
                                                                        (list
                                                                          "https://nonguix-proxy.ditigal.xyz")
                                                                        %default-substitute-urls))
                                                      (authorized-keys (append
                                                                        (list (local-file
                                                                                "./nonguix-signing-key.pub"))
                                                                        %default-authorized-guix-keys)))))))

    (file-systems (append (list
                           (file-system
                             (device (file-system-label "GNU-ESP"))
                             (mount-point "/boot/efi")
                             (type "vfat"))
                           (file-system
                             (device (file-system-label root-label))
                             (mount-point "/")
                             (type "btrfs")))
                        %base-file-systems))

    (bootloader (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets '("/boot/efi"))
      (timeout 0)))))

guldan
