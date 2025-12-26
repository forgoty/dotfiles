(define-module (forgoty systems base-system)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu services base)
  #:use-module (guix store)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (forgoty services sunshine)
  #:use-module ((forgoty substitute-keys) #:prefix substitute-keys:)
  #:use-module ((forgoty build-machines) #:prefix build-machines:)
  #:export (default-user-account default-keyboard-layout
                                 default-system-packages
                                 default-system-services base-operating-system
                                 %default-username))

(define %default-username "nikita")

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
    (name %default-username)
    (comment (string-capitalize %default-username))
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

(define sudoers-file
  (plain-file "sudoers"
    (string-append
     (plain-file-content %sudoers-specification)
     "\n"
     "Cmnd_Alias LOGINCTL = /run/current-system/profile/bin/loginctl\n"
     "Cmnd_Alias SLOCK = /home/nikita/.guix-home/profile/bin/slock\n"
     "Cmnd_Alias MOUNT = /run/privileged/bin/mount,/run/privileged/bin/umount\n"
     "Cmnd_Alias BRIGHTNESS = /run/current-system/profile/bin/brightnessctl\n"
     "\n"
     (format #f "~a ALL=(ALL) NOPASSWD: LOGINCTL,SLOCK,MOUNT,BRIGHTNESS\n" %default-username))))


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
	(service sunshine-service-type)
	(service iptables-service-type)
	(service rootless-podman-service-type
		 (rootless-podman-configuration
		   (podman #f)
		   (subgids
		     (list (subid-range (name %default-username))))
		   (subuids
		     (list (subid-range (name %default-username))))))))

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
                                                                         "https://nonguix-proxy.ditigal.xyz"
                                                                         "http://192.168.100.100:5556")
                                                                        %default-substitute-urls))
                                                      (authorized-keys (append
                                                                        (list substitute-keys:nonguix.pub
                                                                              substitute-keys:guldan.pub)
                                                                        %default-authorized-guix-keys))
                                                      (build-machines (list build-machines:guldan-build-machine))
                                                      (extra-options
                                                        (list "--gc-keep-derivations=yes"
                                                              "--gc-keep-outputs=yes")))))))

    ;; The bootloader and file-systems fields here will be replaced by
    ;; actual operating system configuration
    (file-systems (cons (file-system
                          (device "my-root")
                          (mount-point "/")
                          (type "ext4")) %base-file-systems))
    (bootloader (bootloader-configuration
                  (bootloader grub-bootloader)
                  (targets '("/dev/sda"))))))
