(define-module (forgoty systems t495)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (forgoty systems base-system))

(operating-system
  (inherit base-operating-system)
  (host-name "t495")

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
  (mapped-devices (list (mapped-device
                          (source (uuid "2a8b9891-7d2e-44ec-ada9-c681d353678b"))
                          (target "guix")
                          (type luks-device-mapping))))

  (file-systems (cons* (file-system
                         (mount-point "/")
                         (device "/dev/mapper/guix")
                         (type "btrfs")
                         (dependencies mapped-devices))
                       (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "8A1C-5069"
                                       'fat32))
                         (type "vfat")) %base-file-systems))

  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (menu-entries (list (menu-entry (label "windows")
                                                (device (uuid "8A1C-5069"
                                                              'fat))
                                                (chain-loader
                                                 "/efi/Microsoft/Boot/bootmgfw.efi")))))))
