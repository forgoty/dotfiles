(define-module (forgoty home guldan)
  #:use-module (gnu home)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages file)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages moreutils)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages video)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages suckless)
  #:use-module (gnu services xorg)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services mcron)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services shells)
  #:use-module (forgoty home services sunshine)
  #:use-module (forgoty home services containers)
  #:use-module ((forgoty systems base-system) #:select (default-keyboard-layout)))

(define-public jellyfin-compose-file
  (let* ((docker-compose-jellyfin-service
    '("jellyfin"
        ("network_mode" . "host")
        ("image" . "jellyfin/jellyfin:10")
        ("container_name" . "jellyfin")
        ("environment" . #("TZ=Europe/Warsaw"))
        ("volumes" . #("/home/nikita/Jellyfin Server Media:/media")))))

  (computed-file "jellyfin-docker-compose.json"
    (with-extensions (list guile-json-4)
      (with-imported-modules (source-module-closure '((json builder)))
        #~(begin
            (use-modules (json builder))
            (with-output-to-file #$output
              (lambda ()
                (scm->json
                `(("version" . "3.8")
                  ("services"
                    #$docker-compose-jellyfin-service)))))))))))

(define (home-guldan-dotfiles-configuration config)
  (append
    (home-dotfiles-configuration->files (home-dotfiles-configuration
          (excluded '("README.md"
                ".stow-local-ignore"
                ".xinitrc"
                ".xprofile"
                ".emacs.d"))
          (directories '("../../../dotfiles"))))
    `(( ".xinitrc" ,(plain-file "xinitrc" "exec xterm")))))

(define home-guldan-dotfiles-service-type
  (service-type (name 'home-dotfiles)
                (description "Dotfiles configuration")
                (extensions (list (service-extension home-files-service-type
                                                     home-guldan-dotfiles-configuration)
                                  (service-extension home-shell-profile-service-type
                                                     (const (list (plain-file
                                                                   "startx"
                                                                   "[ $(tty) = /dev/tty1 ] && exec startx"))))))
                (default-value #f)))

(define-public guldan-home
  (home-environment
   (packages (list
              ;; Xorg
              util-linux
              xdg-utils
              xdg-desktop-portal-gtk
              setxkbmap
              xset
              xmodmap
              xrandr
              xrdb
              xauth
              xprop
              xwininfo
              xf86-input-joystick
              lm-sensors
              xdotool
              xclip
              xcompmgr
              font-google-noto
              font-google-noto-emoji
              dwm
              xterm

              ;; Shell
              zsh-syntax-highlighting
              zsh-completions
              file
              bat
              lf
              git
              wget
              fzf
              maim
              ripgrep
              zip
              moreutils
              unzip
              htop
              rsync
              pulsemixer
              dosfstools
              curl
              mediainfo

              ;; Apps
              flatpak
              mpv))

    (services (append %base-home-services
                      (list
                        ;; Pipewire
                        (service home-pipewire-service-type)

                        ;; Shell
                        (service home-zsh-service-type)

                        ;; Dotfiles
                        (service home-guldan-dotfiles-service-type)

                        (service home-dbus-service-type)

                        (service home-startx-command-service-type
                                  (xorg-configuration (keyboard-layout
                                                      default-keyboard-layout)))

                        (service home-x11-service-type)

                        (service home-sunshine-service-type
                                (home-sunshine-configuration
                                  (config-file-path "~/.config/sunshine/sunshine.conf")))

                        ;; Podman
                        (service podman-service-type)
                        (service podman-compose-service-type
                              (podman-compose-configuration
                                (compose-file jellyfin-compose-file)
                                (project-name "jellyfin")
                                (requirement '())
                                (respawn? #t))
                              ))))))
