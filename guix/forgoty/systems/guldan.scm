(define-module (forgoty systems guldan)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system image)
  #:use-module (gnu services base)
  #:use-module (gnu services guix)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (nongnu services nvidia)
  #:use-module (forgoty services sunshine)
  #:use-module (forgoty home guldan)
  #:use-module ((forgoty systems base-system) #:select (%default-username)))

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
(define root-fs
  (file-system
    (device (file-system-label root-label))
    (mount-point "/")
    (type "btrfs")))

(define efi-fs
  (file-system
    (device (file-system-label "GNU-ESP"))
    (mount-point "/boot/efi")
    (type "vfat")))

(define sudoers-file
  (plain-file "sudoers"
              (string-append
               (plain-file-content %sudoers-specification)
               (format #f "~a ALL = NOPASSWD: ALL~%" %default-username))))

(define system-packages
  (list bluez
        bluez-alsa
        fuse-exfat
        ntfs-3g
        xf86-video-dummy
        libva
        libva-utils
        neovim))

(define system-services
  (list (service openssh-service-type
                 (openssh-configuration (port-number 2222)))
        ;;(service dhcpcd-service-type)
        (service bluetooth-service-type
                 (bluetooth-configuration (auto-enable? #t)))
        (service libvirt-service-type
                 (libvirt-configuration (unix-sock-group "libvirt")))
        (service sunshine-service-type)
        (service iptables-service-type)
        (service nvidia-service-type)
        (service guix-home-service-type `((,%default-username ,guldan-home)))
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
    (sudoers-file sudoers-file)
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
                                                      (authorize-key? #f)
                                                      (substitute-urls (append
                                                                        (list
                                                                          "https://nonguix-proxy.ditigal.xyz")
                                                                        %default-substitute-urls))
                                                      (authorized-keys (append
                                                                        (list (local-file
                                                                                "./nonguix-signing-key.pub"))
                                                                        %default-authorized-guix-keys)))))))

    (file-systems (append (list efi-fs root-fs)
                        %base-file-systems))

    (bootloader (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets '("/boot/efi"))
      (timeout 0)))))

guldan
