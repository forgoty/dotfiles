(define-module (forgoty systems guldan)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system image)
  #:use-module (gnu services base)
  #:use-module (gnu services guix)
  #:use-module (gnu services sysctl)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu packages firmware)
  #:use-module (nongnu packages nvidia)
  #:use-module (nongnu packages wine)
  #:use-module (nongnu system linux-initrd)
  #:use-module (nongnu services nvidia)
  #:use-module (nonguix utils)
  #:use-module (nonguix transformations)
  #:use-module (forgoty services sunshine)
  #:use-module (forgoty home guldan)
  #:use-module (forgoty packages nvidia-container)
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
                     networking
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

(define 512gb-disk-fs
  (file-system
    (device (file-system-label "512gb-disk"))
    (mount-point "/media/jellyfin")
    (type "xfs")))

(define sudoers-file
  (plain-file "sudoers"
              (string-append
               (plain-file-content %sudoers-specification)
               (format #f "~a ALL = NOPASSWD: ALL~%" %default-username))))

(define network-adapter-priority-file
  ;; Give higher priority to WiFi over Ethernet Powerline
  (plain-file "network-adapter-priority.conf"
    (string-append
      "[connection-ethernet]\n"
      "match-device=type:ethernet\n"
      "ipv4.route-metric=600\n"
      "ipv6.route-metric=600\n"
      "ipv4.never-default=yes\n"
      "ipv6.never-default=yes\n"
      "connection.autoconnect-priority=-999\n"
      "\n"
      "[connection-wifi]\n"
      "match-device=type:wifi\n"
      "ipv4.route-metric=100\n"
      "ipv6.route-metric=100\n")))

(define wifi-powersave-off-file
  ;; Disable WiFi power saving
  (plain-file "wifi-powersave-off.conf"
      (string-append
        "[connection]\n"
        "wifi.powersave = 2\n")))

(define system-packages
  (list bluez
        bluez-alsa
        fuse-exfat
        ntfs-3g
        xf86-video-dummy
        libva
        libva-utils
        dxvk-next
        nvidia-container-toolkit
        neovim))

(define system-services
  (list (service openssh-service-type
                 (openssh-configuration (port-number 2222)))
        ;;(service dhcpcd-service-type)
        (service bluetooth-service-type
                 (bluetooth-configuration (auto-enable? #t)))
        (udev-rules-service 'wol
          (file->udev-rule "91-wol.rules"
            (mixed-text-file "91-wol.rules"
              ;; https://wiki.archlinux.org/title/Wake-on-LAN
              #~(string-join
                  (list
                    "ACTION==\"add\""
                    "SUBSYSTEM==\"net\""
                    "NAME==\"en*\""
                    (format #f "RUN+=\"~a/sbin/ethtool -s $name wol g\"~%" #$ethtool))))))
        (service guix-publish-service-type
          (guix-publish-configuration
            (host "0.0.0.0")
            (port 5556)
            (ttl (* 90 24 3600))))
        (service libvirt-service-type
                 (libvirt-configuration (unix-sock-group "libvirt")))
        (service sunshine-service-type)
        (service iptables-service-type)
        (service guix-home-service-type `((,%default-username ,guldan-home)))
        (service rootless-podman-service-type
          (rootless-podman-configuration
            (podman #f)
            (subgids
              (list (subid-range (name %default-username))))
            (subuids
              (list (subid-range (name %default-username))))))))

(define-public guldan
  ((compose (nonguix-transformation-nvidia))
    (operating-system
      (locale "en_US.utf8")
      (timezone "Europe/Warsaw")
      (host-name "guldan")
      (kernel linux)
      (initrd microcode-initrd)
      (firmware (list linux-firmware))
      (sudoers-file sudoers-file)
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
                  (network-manager-service-type config =>
                    (network-manager-configuration
                      (inherit config)
                      (extra-configuration-files
                        `(("wifi-powersave-off.conf" ,wifi-powersave-off-file)
                          ("network-adapter-priority.conf" ,network-adapter-priority-file)))))
                  (sysctl-service-type config =>
                    (sysctl-configuration
                      (settings
                        (append
                          '(("net.ipv4.conf.all.arp_ignore" . "1")
                            ("net.ipv4.conf.all.arp_announce" . "2"))
                          %default-sysctl-settings))))
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
                                                                          %default-authorized-guix-keys))
                                                        (extra-options
                                                          (list "--gc-keep-derivations=yes"
                                                                "--gc-keep-outputs=yes")))))))

      (file-systems (append (list efi-fs root-fs 512gb-disk-fs)
                          %base-file-systems))

      (bootloader (bootloader-configuration
        (bootloader grub-efi-bootloader)
        (targets '("/boot/efi"))
        (timeout 0))))))

guldan
