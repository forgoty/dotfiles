(define-module (forgoty systems guldan)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu services base)
  #:use-module (gnu services guix)
  #:use-module (gnu packages linux)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  ;; #:use-module (nongnu services nvidia)
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
    (version "1.14.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/DisplayLink/evdi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ysllh2qnkllvq7k1w2gpn6qqszra08s4ywsqcppzzbd1wgc07f4"))))
    (arguments
     (list #:tests? #f
           #:linux linux
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'chdir
                          (lambda _
                            (chdir "module"))))))))

(define system-packages
  (list bluez
        bluez-alsa
        evdi-linux
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
        ;; (service nvidia-service-type)
        (service guix-home-service-type `(("nikita" ,guldan-home)))
        (service kernel-module-loader-service-type '("evdi"))
        (simple-service 'evdi-config etc-service-type
                        (list `("modprobe.d/evdi.conf"
                                ,(plain-file "evdi.conf"
                                             "options evdi initial_device_count=1"))))
        (service rootless-podman-service-type
          (rootless-podman-configuration
            (podman #f)
            (subgids
              (list (subid-range (name "nikita"))))
            (subuids
              (list (subid-range (name "nikita"))))))))

(define guldan
  (operating-system
    (locale "en_US.utf8")
    (timezone "Europe/Warsaw")
    (host-name "guldan")
    (kernel linux)
    (initrd microcode-initrd)
    (firmware (list linux-firmware))
    (kernel-loadable-modules (list evdi-linux))
    (users (append
            (list (user-account
              (name "nikita")
              (comment "Nikita")
              (password (crypt "password" "$6$abc"))
              (group "users")
              (home-directory "/home/nikita")
              (shell (file-append zsh "/bin/zsh"))
              (supplementary-groups '("wheel"
                                      "netdev"
                                      "audio"
                                      "input"
                                      "tty"
                                      "video"
                                      "lp"
                                      "cgroup"
                                      "kvm"))))
            %base-user-accounts))
    (packages (append system-packages %base-packages))
    (services
      (append system-services
              (modify-services %desktop-services
                (delete gdm-service-type)
                (mingetty-service-type config =>
                  (mingetty-configuration
                    (inherit config)
                    (auto-login "nikita")))
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
                             (device "/dev/vda1")
                             (mount-point "/")
                             (type "btrfs")))
                        %base-file-systems))
    (bootloader (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets '("/boot/efi"))
      (timeout 0)))))

guldan
