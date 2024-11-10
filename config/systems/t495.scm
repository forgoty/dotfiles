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

(use-service-modules avahi admin cups dbus xorg desktop linux docker audio ssh virtualization networking pm)
(use-package-modules
  admin fonts linux audio file-systems vim nfs ssh shells
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

  (services (cons*
	      (service slim-service-type
		       (slim-configuration
			 (auto-login? #t)
			 (display ":0")
			 (vt "vt1")
			 (xorg-configuration
			   (xorg-configuration
			     (keyboard-layout keyboard-layout)))
			 (default-user "nikita")))
	      (service docker-service-type)
	      (service containerd-service-type)
	      (service bluetooth-service-type
		       (bluetooth-configuration
			 (auto-enable? #t)))
	      (service libvirt-service-type
		       (libvirt-configuration (unix-sock-group "libvirt")))
	      (service openssh-service-type
		       (openssh-configuration
			 (openssh openssh-sans-x)
			 (port-number 2222)))
	      (modify-services %desktop-services
		      (delete gdm-service-type))))

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
