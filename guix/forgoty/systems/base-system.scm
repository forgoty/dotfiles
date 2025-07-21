(define-module (forgoty systems base-system)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu services base)
  #:use-module (guix store)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:export (default-user-account default-keyboard-layout
                                 default-system-packages
                                 default-system-services base-operating-system))

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
                     package-management)

(define default-user-account
  (user-account
    (name "nikita")
    (comment "Nikita")
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
                            "kvm"
                            "libvirt"))))

(define sudoers-file
  (plain-file "sudoers"
   "# aliases
Cmnd_Alias LOGINCTL = /run/current-system/profile/bin/loginctl
Cmnd_Alias SLOCK = /home/nikita/.guix-home/profile/bin/slock
Cmnd_Alias MOUNT = /run/privileged/bin/mount,/run/privileged/bin/umount
Cmnd_Alias BRIGHTNESS = /run/current-system/profile/bin/brightnessctl

root ALL=(ALL) ALL
%wheel ALL=(ALL) ALL
nikita ALL=(ALL) NOPASSWD: LOGINCTL,SLOCK,MOUNT,BRIGHTNESS
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
  (list (service openssh-service-type
                 (openssh-configuration (port-number 2222)))
        (service bluetooth-service-type
                 (bluetooth-configuration (auto-enable? #t)))
        (service virtlog-service-type)
        (service libvirt-service-type
                 (libvirt-configuration (unix-sock-group "libvirt")))
	(service iptables-service-type)
	(service rootless-podman-service-type
		 (rootless-podman-configuration
		   (podman #f)
		   (subgids
		     (list (subid-range (name "nikita"))))
		   (subuids
		     (list (subid-range (name "nikita"))))))))

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
               (delete gdm-service-type)
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

    ;; The bootloader and file-systems fields here will be replaced by
    ;; actual operating system configuration
    (file-systems (cons (file-system
                          (device "my-root")
                          (mount-point "/")
                          (type "ext4")) %base-file-systems))
    (bootloader (bootloader-configuration
                  (bootloader grub-bootloader)
                  (targets '("/dev/sda"))))))
