(define-module (forgoty systems base-system)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:export (default-user-account default-keyboard-layout
                                 default-system-packages
                                 default-system-services base-operating-system))

(use-service-modules desktop
                     linux
                     docker
                     xorg
                     audio
                     virtualization)
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
                     package-management)

(define default-user-account
  (user-account
    (name "nikita")
    (comment "Nikita")
    (group "users")
    (home-directory "/home/nikita")
    (shell (file-append zsh "/bin/zsh"))
    (supplementary-groups '("wheel" "netdev"
                            "audio"
                            "docker"
                            "input"
                            "tty"
                            "video"
                            "lp"))))

(define sudoers-file
  (plain-file "sudoers"
   "# aliases
Cmnd_Alias HALT = /run/current-system/profile/sbin/reboot, /run/current-system/profile/sbin/halt, /run/current-system/profile/sbin/shutdown
Cmnd_Alias LOGINCTL = /run/current-system/profile/bin/loginctl
Cmnd_Alias SLOCK = /home/nikita/.guix-home/profile/bin/slock

root ALL=(ALL) ALL
%wheel ALL=(ALL) ALL
nikita ALL=NOPASSWD: HALT, LOGINCTL, SLOCK
"))

(define default-keyboard-layout
  (keyboard-layout "pl,ru"
                   #:options '("grp:shifts_toggle" "caps:escape")))

(define default-system-packages
  (list bluez
        bluez-alsa
        brightnessctl
        fuse-exfat
        gvfs
        ntfs-3g
        libva-utils
        vim))

(define default-system-services
  (list (service slim-service-type
                 (slim-configuration (auto-login? #t)
                                     (vt "vt1")
                                     (xorg-configuration (xorg-configuration (keyboard-layout
                                                                              default-keyboard-layout)))
                                     (default-user "nikita")))
        (service docker-service-type)
        (service containerd-service-type)
        (service bluetooth-service-type
                 (bluetooth-configuration (auto-enable? #t)))
        (service libvirt-service-type
                 (libvirt-configuration (unix-sock-group "libvirt")))))

;; This is a base operating system that must be inherited in actual host config
(define base-operating-system
  (operating-system
    (locale "en_US.utf8")
    (timezone "Europe/Warsaw")
    (keyboard-layout default-keyboard-layout)
    (kernel linux)
    (initrd microcode-initrd)
    (firmware (list linux-firmware))
    (host-name "guix")
    (users (cons* default-user-account %base-user-accounts))
    (sudoers-file sudoers-file)
    (packages (append default-system-packages %base-packages))
    (services
     (append default-system-services
             (modify-services %desktop-services
               (delete gdm-service-type))))

    ;; The bootloader and file-systems fields here will be replaced by
    ;; actual operating system configuration
    (file-systems (cons (file-system
                          (device "my-root")
                          (mount-point "/")
                          (type "ext4")) %base-file-systems))
    (bootloader (bootloader-configuration
                  (bootloader grub-bootloader)
                  (targets '("/dev/sda"))))))
