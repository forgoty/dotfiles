;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.


;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules (gnu)
	     (nongnu packages linux)
	     (gnu system)
	     (gnu system nss)
	     (gnu system setuid)
	     (nongnu system linux-initrd))

(use-service-modules avahi admin cups dbus desktop linux docker audio ssh virtualization networking pm)
(use-package-modules
  fonts linux audio file-systems vim nfs ssh shells
  gnome video package-management version-control)

(operating-system
  (locale "en_US.utf8")
  (timezone "Europe/Warsaw")
  (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))

  (keyboard-layout (keyboard-layout "pl,ru"
				    #:model "thinkpad"
				    #:options '("grp:shifts_toggle" "caps:escape")))
  (host-name "t495")

  ;; The list of user accounts ('root' is implicit).
  (users (cons* (user-account
                  (name "nikita")
                  (comment "Nikita")
                  (group "users")
                  (home-directory "/home/nikita")
		  (shell (file-append zsh "/bin/zsh"))
                  (supplementary-groups '(
					  "wheel"
					  "netdev"
					  "audio"
					  "docker"
					  "input"
					  "tty"
					  "video"
					  "lp")))
                %base-user-accounts))

  (packages
    (cons* bluez
	   bluez-alsa
	   brightnessctl
	   exfat-utils
	   fuse-exfat
	   gvfs
	   ntfs-3g
	   libva-utils
	   vim
	   stow
	   git
	%base-packages))

  (services
    (append
      (modify-services %base-services
	(delete console-font-service-type)
	(delete login-service-type)
	(delete mingetty-service-type))

      (list
	(service elogind-service-type)
      ;; Network
      (service network-manager-service-type
	       (network-manager-configuration
		 (vpn-plugins
		   (list network-manager-openvpn))))
      (service wpa-supplicant-service-type)
      (service modem-manager-service-type)
      (service bluetooth-service-type
	       (bluetooth-configuration
		 (auto-enable? #t)))
      (service usb-modeswitch-service-type)

      ;; Basic
      (service avahi-service-type)
      (service udisks-service-type)
      (service upower-service-type)
      (service cups-pk-helper-service-type)
      (service geoclue-service-type)
      (service polkit-service-type)
      (service dbus-root-service-type)
      (service openssh-service-type)
      (service ntp-service-type)
      fontconfig-file-system-service

      (service console-font-service-type
	       (map (lambda (tty)
		      (cons tty (file-append font-terminus "/share/consolefonts/ter-132n")))
		    '("tty1" "tty2" "tty3" "tty4" "tty5" "tty6")))

      ;; Docker
      (service docker-service-type)
      (service libvirt-service-type
	(libvirt-configuration (unix-sock-group "libvirt")))

      (service greetd-service-type
	(greetd-configuration
	  (greeter-supplementary-groups (list "video" "input"))
	   (terminals
	     (list
	       (greetd-terminal-configuration
		 (terminal-vt "1")
		 (terminal-switch #t)
		 (default-session-command
		   (greetd-agreety-session (command (file-append zsh "/bin/zsh")))))
	       (greetd-terminal-configuration (terminal-vt "2"))
	       (greetd-terminal-configuration (terminal-vt "3"))
	       (greetd-terminal-configuration (terminal-vt "4"))
	       (greetd-terminal-configuration (terminal-vt "5"))
	       (greetd-terminal-configuration (terminal-vt "6"))))))

      ;; Power and thermal management services
      (service thermald-service-type)
      (service tlp-service-type
	(tlp-configuration
	  (cpu-boost-on-ac? #t)
	  (wifi-pwr-on-bat? #t)))

      ;; Cups
      (service cups-service-type
	       (cups-configuration
		 (web-interface? #t)))

     )))

  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)
		(menu-entries
		  (list (menu-entry
			  (label "windows")
			  (device (uuid "8A1C-5069" 'fat))
			  (chain-loader "/efi/Microsoft/Boot/bootmgfw.efi"))))))
  (mapped-devices (list (mapped-device
                          (source (uuid
                                   "2a8b9891-7d2e-44ec-ada9-c681d353678b"))
                          (target "guix")
                          (type luks-device-mapping))))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
  (file-systems (cons* (file-system
                         (mount-point "/")
                         (device "/dev/mapper/guix")
                         (type "btrfs")
                         (dependencies mapped-devices))
                       (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "8A1C-5069"
                                       'fat32))
                         (type "vfat")) %base-file-systems)))
