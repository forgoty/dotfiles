;; $(guix system -L ~/.dotfiles/guix vm ~/.dotfiles/guix/forgoty/systems/sunshine-vm.scm --no-offload) -nic user,model=virtio-net-pci,hostfwd=tcp::2200-:22,hostfwd=tcp::49984-:47984,hostfwd=tcp::49989-:47989,hostfwd=tcp::49990-:47990,hostfwd=udp::49998-:47998,hostfwd=tcp::49999-:47999,hostfwd=udp::50000-:48000,hostfwd=tcp::50010-:48010

(define-module (forgoty systems sunshine-vm)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services xdg)
  #:use-module (gnu services base)
  #:use-module (gnu services guix)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu packages firmware)
  #:use-module (nongnu packages game-client)   ; steam
  #:use-module (nongnu system linux-initrd)
  #:use-module (forgoty services sunshine)
  #:use-module (forgoty home services sunshine)
  #:use-module (forgoty home services desktop)
  #:use-module ((forgoty systems base-system) #:select (%default-username)))

;; `xorg` is required here even though this is a Wayland/labwc guest: it is
;; where `gdm-service-type` is actually defined (NOT `(gnu services
;; desktop)`), and `%desktop-services` + `(delete gdm-service-type)` below
;; needs that binding in scope at macro-expansion time.
(use-service-modules desktop xorg ssh networking)
;; NOTE: deviates slightly from the originally suggested list -- verified via
;; `guix show <package>` against the actual upstream locations:
;;   - util-linux, adwaita-icon-theme/hicolor-icon-theme live in
;;     gnu/packages/linux.scm and gnu/packages/gnome.scm respectively (not
;;     covered by the original gl/video/wm/freedesktop/fonts/shells list), so
;;     `linux` and `gnome` were added.
;;   - moonlight-qt lives in gnu/packages/games.scm (it is a free/libre
;;     client, unlike proprietary `steam`), so `games` was added.
;;   - wl-clipboard lives in gnu/packages/xdisorg.scm, so `xdisorg` was added.
;;   - vim lives in gnu/packages/vim.scm, so `vim` was added.
;; This mirrors the actual working imports used by guldan.scm / home/guldan.scm.
(use-package-modules gl video wm freedesktop fonts shells
                     gnome linux vim games xdisorg)

;; =============================================================================
;; sunshine-vm: dedicated Sunshine game-streaming guest VM
;; =============================================================================
;;
;; PURPOSE
;; -------
;; This defines a QEMU guest `operating-system` (plus an in-file
;; `home-environment`) for a VM whose sole job is running the Sunshine
;; streaming host so a GPU can be passed through to it and streamed to
;; Moonlight clients. It is a sibling configuration to the "guldan" host
;; (see systems/guldan.scm) but isolated into its own guest so that GPU
;; passthrough, port remapping, and directory sharing can be configured
;; independently without touching guldan's native/bare-metal Sunshine setup.
;;
;; GPU PASSTHROUGH IS A HOST-SIDE CONCERN
;; ---------------------------------------
;; From inside this guest, a VFIO-passed-through GPU just looks like an
;; ordinary PCI device, so this operating-system only needs normal GPU
;; userspace libraries (mesa, libva, libva-utils -- see `system-packages`
;; below). All of the actual VFIO plumbing happens on the HOST that will run
;; `qemu-system-x86_64`, and must be configured on that host's own
;; operating-system (e.g. guix/forgoty/systems/guldan.scm), NOT here:
;;
;;   1. Enable IOMMU/VT-d in the host's BIOS/UEFI firmware settings.
;;   2. Add IOMMU kernel arguments to the HOST's `operating-system` (via
;;      `kernel-arguments`), e.g.:
;;        - AMD:   "amd_iommu=on" "iommu=pt"
;;        - Intel: "intel_iommu=on" "iommu=pt"
;;   3. Bind the target GPU (and its HDMI/DP audio function) to `vfio-pci`
;;      early in boot, on the HOST, by adding a `vfio-pci.ids=XXXX:YYYY,XXXX:ZZZZ`
;;      kernel argument (VGA device id, audio device id) and ensuring
;;      `vfio`, `vfio_iommu_type1`, and `vfio_pci` are present in the HOST's
;;      `initrd-modules` (loaded before amdgpu/nouveau/i915 claim the card).
;;   4. Discover the real PCI vendor:device IDs and bus/device/function
;;      (BDF) addresses on the host with:
;;        lspci -nnk | grep -A3 VGA
;;      and use the resulting `<GPU_BDF>` / `<GPU_AUDIO_BDF>` values (e.g.
;;      "0000:03:00.0" / "0000:03:00.1") in the `qemu` invocation at the
;;      bottom of this file.
;;
;; PORT REMAP TABLE
;; -----------------
;; IMPORTANT CORRECTION: an earlier revision of this file kept Sunshine's
;; *guest-internal* ports at their stock defaults (47984-48010) and only
;; shifted them externally via QEMU `hostfwd` NAT translation (guest 47989
;; -> host 49989, etc). That does NOT work end-to-end with Moonlight
;; clients (moonlight-qt included): after the initial handshake, Sunshine's
;; own HTTP/RTSP responses embed the port numbers *it* is internally
;; configured with (its own `port` setting, still 47989-based) inside URLs
;; the client is told to connect to next (RTSP/video/audio/control).
;; Moonlight does not independently recompute those from whatever
;; address:port you typed in "Add PC manually" -- it mostly hardcodes the
;; non-base ports or reads them verbatim from the server's own responses
;; (see e.g. moonlight-qt issues #145, #427, #514, #731, #927 -- port
;; configurability beyond the single entry-point port is a long-standing,
;; still-open feature request upstream). So a plain NAT shift only ever
;; gets you through the *first* handshake; the actual RTSP/video/audio/
;; control connections would then target the guest's real 47984-48010
;; range on the host IP, which is NOT what we forward, and which may
;; already be guldan's own native Sunshine on that same host.
;;
;; THE FIX: make the GUEST's own Sunshine actually listen on the offset
;; ports itself, by seeding a full sunshine.conf whose first line is
;; "port = 49989" (see `sunshine-vm-sunshine-conf`, a `plain-file` with its
;; content written directly as Scheme source below, wired in via the
;; `config-template` field of `home-sunshine-configuration`,
;; forgoty/home/services/sunshine.scm). That field one-time-*copies*
;; config-template's content into config-file-path the very first time
;; Sunshine runs, without ever touching the file again afterwards so
;; pairing state / later Web UI changes are never clobbered (a copy, not a
;; symlink into the read-only store, so the live file stays writable).
;; Since Sunshine itself then computes every other port as a fixed offset
;; from that same 49989 base -- exactly like it would from 47989 -- the
;; final numbers work out identical to the earlier (NAT-shifted) table
;; below, but now they are the guest's *real*, natively-bound ports, so
;; `hostfwd` only needs a plain 1:1 passthrough (no translation), and
;; whatever URLs Sunshine reports to Moonlight already match what's
;; actually forwarded:
;;
;;   | Function          | Guest port (native) | Host-forwarded port |
;;   |--------------------|---------------------|----------------------|
;;   | GameStream HTTPS   | 49984/tcp           | 49984/tcp (1:1)      |
;;   | GameStream HTTP    | 49989/tcp           | 49989/tcp (1:1)      |
;;   | Web UI HTTPS       | 49990/tcp           | 49990/tcp (1:1)      |
;;   | Video              | 49998/udp           | 49998/udp (1:1)      |
;;   | Control            | 49999/tcp           | 49999/tcp (1:1)      |
;;   | Audio              | 50000/udp           | 50000/udp (1:1)      |
;;   | RTSP               | 50010/tcp           | 50010/tcp (1:1)      |
;;   | SSH                | 22/tcp              | 2200/tcp             |
;;
;; This still keeps guldan's own native Sunshine (47984-48010, unmodified)
;; conflict-free on the same physical host, since this VM's Sunshine never
;; touches that range at all -- neither internally nor externally.
;;
;; IMPORTANT: SLIRP hostfwd breaks Moonlight's LAN mDNS auto-discovery, so
;; clients must add this VM manually as <host-IP>:<port> (e.g. the web UI
;; at https://<host-IP>:49990, and "Add PC manually" using <host-IP>:49989
;; for pairing) rather than relying on auto-discovery. A production setup
;; should instead use bridged/tap networking, giving the VM its own LAN IP
;; -- in that case `sunshine-vm-sunshine-conf`'s "port" line below can be
;; left at its default (no remap needed at all, since guldan and this VM
;; would have distinct IPs).
;;
;; DIRECTORY SHARING
;; ------------------
;; Directory sharing uses virtio-9p (the `9p`/`trans=virtio` file system),
;; NOT virtiofs. `virtiofsd` is not currently packaged anywhere in Guix's
;; package tree (confirmed via package search), so virtiofs is not
;; practically usable here without packaging a new daemon from scratch.
;; Plain virtio-9p works with zero extra host packages, since stock QEMU
;; supports it natively via the built-in `-virtfs` flag. The mount tag
;; "hostshare" configured in `shared-fs` below must match the `mount_tag=`
;; value passed to `-virtfs` on the host (see the run command below).
;;
;; BUILD + RUN
;; -----------
;;   # Quick test (ephemeral `guix system vm`, shared host store, NO GPU
;;   # passthrough and NO persistent disk -- fastest way to sanity-check that
;;   # the system/home services boot, sunshine starts, and ssh/port remaps
;;   # work, before bothering with a real qcow2 image + VFIO passthrough).
;;   # `guix system vm` builds a launcher script; running it with `$(...)`
;;   # executes that script, and any extra arguments (here `-nic ...`) are
;;   # forwarded straight to the underlying `qemu-system-x86_64` invocation.
;;   #
;;   # KNOWN CAVEAT if you try the same recipe against guldan.scm directly:
;;   # `guix system vm` internally builds its throwaway root disk using a
;;   # plain MBR-style image (`raw-with-offset-disk-image`, which never sets
;;   # `partition-table-type`), regardless of the target OS's own bootloader.
;;   # `(gnu system image)` then unconditionally rejects that combination
;;   # whenever the OS bootloader is `grub-efi-bootloader` (as guldan.scm's
;;   # is, since it boots a real GPT+ESP install) with "EFI bootloader
;;   # required with GPT partitioning", and command substitution silently
;;   # expands to an empty string on that failure -- which is why appending
;;   # `-nic ...` afterwards then errors as `zsh: command not found: -nic`
;;   # (zsh is trying to execute the leftover `-nic ...` text as a command
;;   # because `$(...)` produced nothing). This is a `guix system vm`
;;   # limitation unrelated to this file's own configuration.
;;   #
;;   # sunshine-vm.scm avoids this entirely by using a plain (non-EFI)
;;   # `grub-bootloader` targeting `/dev/vda` (see `sunshine-vm-os` below),
;;   # so the same recipe works here -- confirmed via
;;   # `guix system -L ~/.dotfiles/guix vm sunshine-vm.scm --dry-run`, which
;;   # cleanly resolves to a derivation build plan instead of erroring:
;;   $(guix system -L ~/.dotfiles/guix vm ~/.dotfiles/guix/forgoty/systems/sunshine-vm.scm) \
;;     -nic user,model=virtio-net-pci,hostfwd=tcp::2200-:22,hostfwd=tcp::49984-:49984,hostfwd=tcp::49989-:49989,hostfwd=tcp::49990-:49990,hostfwd=udp::49998-:49998,hostfwd=tcp::49999-:49999,hostfwd=udp::50000-:50000,hostfwd=tcp::50010-:50010
;;
;;   # Quick-test verify:
;;   ssh -p 2200 nikita@localhost
;;   curl -k https://localhost:49990   # Sunshine web UI (self-signed cert)
;;
;;   # Full run (persistent disk image + real GPU passthrough via VFIO)
;;   # -------------------------------------------------------------
;;   # Build the qcow2 disk image
;;   guix system image --image-type=qcow2 -L /home/nikita/.dotfiles/guix guix/forgoty/systems/sunshine-vm.scm
;;   cp $(guix system image --image-type=qcow2 -L /home/nikita/.dotfiles/guix guix/forgoty/systems/sunshine-vm.scm) /var/lib/vms/sunshine-vm.qcow2
;;   chmod +w /var/lib/vms/sunshine-vm.qcow2
;;
;;   # Launch with GPU passthrough + 9p share + remapped ports (1:1 passthrough,
;;   # since the guest's own Sunshine now natively binds the 49984-50010 family)
;;   # Replace <GPU_BDF>/<GPU_AUDIO_BDF> with real values from `lspci -nnk`
;;   sudo qemu-system-x86_64 \
;;     -enable-kvm -cpu host -smp 8 -m 16G \
;;     -machine q35,accel=kvm \
;;     -device vfio-pci,host=<GPU_BDF>,multifunction=on \
;;     -device vfio-pci,host=<GPU_AUDIO_BDF> \
;;     -virtfs local,path=/home/nikita/vm-share,mount_tag=hostshare,security_model=mapped-xattr,id=hostshare \
;;     -drive file=/var/lib/vms/sunshine-vm.qcow2,if=virtio,format=qcow2 \
;;     -netdev user,id=net0,hostfwd=tcp::49984-:49984,hostfwd=tcp::49989-:49989,hostfwd=tcp::49990-:49990,hostfwd=udp::49998-:49998,hostfwd=tcp::49999-:49999,hostfwd=udp::50000-:50000,hostfwd=tcp::50010-:50010,hostfwd=tcp::2200-:22 \
;;     -device virtio-net-pci,netdev=net0 \
;;     -vga none -display none \
;;     -daemonize
;;
;;   # Verify
;;   ssh -p 2200 nikita@localhost
;;   curl -k https://localhost:49990   # Sunshine web UI (self-signed cert)
;; =============================================================================

(define %vm-host-name "sunshine-vm")

;; Sunshine's own base "port" config value for this guest (see PORT REMAP
;; TABLE above for the full derived family: base-5/base/base+1/base+9/
;; base+10/base+11/base+21). Baked into `sunshine-vm-sunshine-conf` below,
;; which is seeded into sunshine.conf on first run via
;; `home-sunshine-configuration`'s `config-template` field.
(define %vm-sunshine-base-port 49989)

;; -----------------------------------------------------------------------
;; Declarative configuration: sunshine.conf and labwc's autostart are both
;; plain Scheme file-like objects (`plain-file`), deployed via
;; `home-sunshine-configuration`'s `config-template` field and
;; `home-xdg-configuration-files-service-type` respectively -- no
;; host-directory dotfiles deployment is used for this VM.
;; -----------------------------------------------------------------------

;; Full sunshine.conf content, written directly as Scheme source. Only
;; ever *copied* into place the first time Sunshine runs (see
;; `home-sunshine-configuration`'s `config-template` field below and
;; forgoty/home/services/sunshine.scm) -- Sunshine's own Web UI remains
;; free to rewrite the live file afterwards (pairing state, further
;; settings changes, etc.) without this template ever overwriting it
;; again.
(define sunshine-vm-sunshine-conf
  (plain-file "sunshine.conf"
              (string-append
               "sunshine_name = sunshine-vm\n"
               "capture = wlr\n")))

;; labwc's autostart, written directly as Scheme source (same approach as
;; `sunshine-vm-sunshine-conf` above) and deployed via
;; `home-xdg-configuration-files-service-type` below, at
;; $XDG_CONFIG_HOME/labwc/autostart. guldan's real, working labwc config
;; (dotfiles/guldan/.config/labwc/autostart) has no custom rc.xml/
;; environment -- labwc's built-in default keybindings are used as-is
;; there -- and this VM reuses that same layout, but trimmed down to only
;; launch Steam (guldan's also starts kanshi, a wallpaper setter, and
;; waybar, none of which this headless streaming appliance needs).
(define sunshine-vm-labwc-autostart
  (plain-file "autostart" "steam -tenfoot &\n"))

(define root-fs
  (file-system
    (device (file-system-label "sunshine-vm-root"))
    (mount-point "/")
    (type "ext4")))

;; virtio-9p directory share; mount tag "hostshare" must match the
;; `mount_tag=hostshare` passed to qemu's `-virtfs` flag (see run command
;; above). No host-side package (e.g. virtiofsd) is required.
;;
;; `mount-may-fail? #t` is important: if you boot without a matching
;; `-virtfs ...,mount_tag=hostshare,...` device on the qemu command line
;; (e.g. the "Quick test" `guix system vm` recipe above, which only
;; forwards `-nic ...`), the guest kernel has no virtio-9p channel to
;; attach to and logs `9pnet_virtio: no channels available for device
;; hostshare`. Without `mount-may-fail?`, Shepherd would treat that as a
;; failed/blocking boot-critical mount; with it, the failure is logged and
;; ignored so boot continues normally (sunshine, ssh, etc. all still come
;; up) -- you just won't have /mnt/host-share populated until you run
;; qemu with the matching `-virtfs` flag (see BUILD + RUN below).
(define shared-fs
  (file-system
    (device "hostshare")
    (mount-point "/mnt/host-share")
    (type "9p")
    (options "trans=virtio,version=9p2000.L,msize=104857600")
    (check? #f)
    (mount-may-fail? #t)
    (create-mount-point? #t)))

(define sudoers-file
  (plain-file "sudoers"
              (string-append
               (plain-file-content %sudoers-specification)
               (format #f "~a ALL = NOPASSWD: ALL~%" %default-username))))

;; GPU passthrough note: from the guest's perspective a VFIO-passed-through
;; GPU is just an ordinary PCI device -- the guest only needs ordinary GPU
;; userspace libraries. Real VFIO binding (IOMMU, vfio-pci.ids, vfio kernel
;; modules) is a HOST-side concern for whichever physical machine actually
;; runs qemu, and must NOT be added to this guest's operating-system.
(define system-packages
  (list mesa
        libva
        libva-utils
        vim))

(define sunshine-vm-home-environment-variables
  (append home-default-environment-variables
          (list '("XCURSOR_THEME" . "Adwaita")
                '("XCURSOR_SIZE" . "24"))))

(define sunshine-vm-packages
  (list
   util-linux
   xdg-utils
   xdg-desktop-portal
   xdg-desktop-portal-gtk
   wl-clipboard
   font-google-noto
   font-google-noto-emoji
   adwaita-icon-theme
   hicolor-icon-theme
   labwc
   waybar
   steam
   moonlight-qt))

(define-public sunshine-vm-home
  (home-environment
   (services
    (append %base-home-services
            (list
             (service home-desktop-service-type
                      (home-desktop-configuration
                       (environment-variables sunshine-vm-home-environment-variables)
                       (profile-packages sunshine-vm-packages)
                       (dot-profiles (list (plain-file "labwc"
                                                        "[ $(tty) = /dev/tty1 ] && exec labwc")))
                       (shepherd-services '())))
             (service home-dbus-service-type)
             (service home-pipewire-service-type)

             (service home-xdg-configuration-files-service-type
                      `(("labwc/autostart" ,sunshine-vm-labwc-autostart)
                        ("sunshine/sunshine.conf",sunshine-vm-sunshine-conf)))

             (service home-sunshine-service-type
                      (home-sunshine-configuration
                       (session-type 'wayland)
                       (config-file-path (string-append "/home/" %default-username
                                                         "/.config/sunshine/sunshine.conf")))))))))

(define system-services
  (list (service openssh-service-type
                 (openssh-configuration (port-number 22)))
        (service dhcpcd-service-type)
        (service sunshine-service-type)
        (service guix-home-service-type `((,%default-username ,sunshine-vm-home)))))

(define sunshine-vm-os
  (operating-system
    (locale "en_US.utf8")
    (timezone "Europe/Warsaw")
    (host-name %vm-host-name)
    (kernel linux)
    (initrd microcode-initrd)
    (firmware (list linux-firmware amdgpu-firmware amd-microcode))
    (sudoers-file sudoers-file)
    (users (cons* (user-account
                    (name %default-username)
                    (comment (string-capitalize %default-username))
                    (password (crypt "password" "$6$abc"))
                    (group "users")
                    (home-directory (string-append "/home/" %default-username))
                    (shell (file-append zsh "/bin/zsh"))
                    (supplementary-groups '("wheel" "netdev" "audio" "input" "tty" "video" "lp")))
                  %base-user-accounts))
    (packages (append system-packages %base-packages))
    (services
      (append system-services
              (modify-services %desktop-services
                (delete gdm-service-type)
                (delete network-manager-service-type)
                (mingetty-service-type config =>
                  (mingetty-configuration
                    (inherit config)
                    (auto-login %default-username))))))
    (file-systems (append (list root-fs shared-fs) %base-file-systems))
    (bootloader (bootloader-configuration
      (bootloader grub-bootloader)
      (targets '("/dev/vda"))))))

(define-public sunshine-vm
  sunshine-vm-os)

sunshine-vm
