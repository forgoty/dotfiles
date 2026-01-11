(define-module (forgoty home guldan)
  #:use-module ((srfi srfi-1) #:hide (zip))
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
  #:use-module (gnu packages wm)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages moreutils)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages pantheon)
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
  #:use-module (nongnu packages game-client)
  #:use-module (nongnu packages nvidia)
  #:use-module (nongnu packages video)
  #:use-module (forgoty home services sunshine)
  #:use-module (forgoty home services containers)
  #:use-module (forgoty home services desktop)
  #:use-module (forgoty packages shellutils)
  #:use-module (forgoty packages suckless)
  #:use-module ((forgoty systems base-system) #:select (default-keyboard-layout)))

(define-public jellyfin-compose-file
  (let* ((docker-compose-jellyfin-service
    '("jellyfin"
      ("ports" . #("8096:8096"
                  "8920:8920"
                  "7359:7359/udp"
                  "1900:1900/udp"))
      ("image" . "jellyfin/jellyfin:10")
      ("container_name" . "jellyfin")
      ("environment" . #("TZ=Europe/Warsaw"
                         "NVIDIA_VISIBLE_DEVICES=all"))
      ("devices" . #("nvidia.com/gpu=all"))
      ("depends_on" . #("dispatcharr" "aiostreams"))
      ("restart" . "unless-stopped")
      ("volumes" . #("/media/jellyfin/config/jellyfin:/config"
                    "/media/jellyfin/.cache:/cache"
                    "/media/jellyfin/Shows:/data/tvshows"
                    "/media/jellyfin/Movies:/data/movies"
                    "/media/jellyfin/Downloads:/data/media_downloads"))))
    (docker-compose-jellyseerr-service
    '("jellyseerr"
      ("ports" . #("5055:5055"))
      ("image" . "fallenbagel/jellyseerr:latest")
      ("container_name" . "jellyseerr")
      ("environment" . #("TZ=Europe/Warsaw"
                         "LOG_LEVEL=debug"))
      ("restart" . "unless-stopped")
      ("x-podman.gidmaps" . #("+g911:@998"))
      ("volumes" . #("/media/jellyfin/config/jellyseerr:/app/config"))))
    (docker-compose-radarr-service
    '("radarr"
      ("ports" . #("7878:7878"))
      ("image" . "lscr.io/linuxserver/radarr:latest")
      ("container_name" . "radarr")
      ("environment" . #("TZ=Europe/Warsaw"))
      ("restart" . "unless-stopped")
      ("x-podman.gidmaps" . #("+g911:@998"))
      ("volumes" . #("/media/jellyfin/config/radarr:/config"
                    "/media/jellyfin/Movies:/movies"
                    "/media/jellyfin/Downloads:/downloads"))))
    (docker-compose-sonarr-service
    '("sonarr"
      ("ports" . #("8989:8989"))
      ("image" . "lscr.io/linuxserver/sonarr:latest")
      ("container_name" . "sonarr")
      ("environment" . #("TZ=Europe/Warsaw"))
      ("restart" . "unless-stopped")
      ("x-podman.gidmaps" . #("+g911:@998")) ;; map gid 911 (default group of -arr stack) in container to gid 998 on host (users group)
      ("volumes" . #("/media/jellyfin/config/sonarr:/config"
                    "/media/jellyfin/Shows:/tv"
                    "/media/jellyfin/Downloads:/downloads"))))
    (docker-compose-prowlarr-service
    '("prowlarr"
      ("ports" . #("9696:9696"))
      ("image" . "lscr.io/linuxserver/prowlarr:latest")
      ("container_name" . "prowlarr")
      ("environment" . #("TZ=Europe/Warsaw"))
      ("restart" . "unless-stopped")
      ("x-podman.gidmaps" . #("+g911:@998"))
      ("volumes" . #("/media/jellyfin/config/prowlarr:/config"))))
    (docker-compose-flaresolverr-service
    '("flaresolverr"
      ("ports" . #("8191:8191"))
      ("image" . "ghcr.io/flaresolverr/flaresolverr:latest")
      ("container_name" . "flaresolverr")
      ("environment" . #("TZ=Europe/Warsaw"
                          "LOG_LEVEL=info"
                          "LOG_HTML=false"
                          "CAPTHA_SOLVER=none"))
      ("restart" . "unless-stopped")))
    (docker-compose-qbittorrent-service
    '("qbittorrent"
      ("ports" . #("8080:8080"
                  "6881:6881"
                  "6881:6881/udp"))
      ("image" . "lscr.io/linuxserver/qbittorrent:latest")
      ("container_name" . "qbittorrent")
      ("environment" . #("TZ=Europe/Warsaw"
                         "WEBUI_PORT=8080"))
      ("restart" . "unless-stopped")
      ("x-podman.gidmaps" . #("+g911:@998"))
      ("volumes" . #("/media/jellyfin/config/qbittorrent:/config"
                     "/media/jellyfin/Downloads:/downloads"))))
    (docker-compose-dispatcharr-service
    '("dispatcharr"
      ("image" . "ghcr.io/dispatcharr/dispatcharr:latest")
      ("container_name" . "dispatcharr")
      ("restart" . "unless-stopped")
      ("devices" . #("nvidia.com/gpu=all"))
      ("ports" . #("9191:9191"))
      ("volumes" . #("/media/jellyfin/config/dispatcharr:/data"))
      ("environment" . #("TZ=Europe/Warsaw"))))
    (docker-compose-aiostreams-service
    '("aiostreams"
      ("image" . "ghcr.io/viren070/aiostreams:latest")
      ("container_name" . "aiostreams")
      ("restart" . "unless-stopped")
      ("depends_on" . #("prowlarr"))
      ("ports" . #("7676:3000"))
      ("volumes" . #("/media/jellyfin/config/aiostreams:/app/data"))
      ("x-podman.gidmaps" . #("+g1000:@998"))
      ("environment" . #("SECRET_KEY=43ef3cdf9c3a1477158d0dfd054125d5f400e5fa6fcb55880fb3a94b68c1a4f6"
                        "BASE_URL=http://192.168.100.100:7676"
                        "INTERNAL_URL=http://192.168.100.100:7676"
                        "DATABASE_URI=sqlite://./data/db.sqlite"
                        "BUILTIN_PROWLARR_URL=http://192.168.100.100:9696"
                        "BUILTIN_PROWLARR_API_KEY=2f32d7f825f04b27b9f7f695d89ff6e6"
                        "DISABLE_RATE_LIMITS=true"
                        "LOG_TIMEZONE=Europe/Warsaw"))
      ("healthcheck"
        ("test" . "wget -qO- http://localhost:3000/api/v1/status")
        ("interval" . "1m")
        ("timeout" . "10s")
        ("retries" . 5)
        ("start_period" . "10s")))))

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
                   #$docker-compose-jellyfin-service
                   #$docker-compose-jellyseerr-service
                   #$docker-compose-radarr-service
                   #$docker-compose-sonarr-service
                   #$docker-compose-prowlarr-service
                   #$docker-compose-flaresolverr-service
                   #$docker-compose-aiostreams-service
                   #$docker-compose-dispatcharr-service
                   #$docker-compose-qbittorrent-service)))))))))))

(define (home-guldan-dotfiles-configuration config)
  (append
    (home-dotfiles-configuration->files (home-dotfiles-configuration
          (excluded '("README.md"
                ".stow-local-ignore"
                ".xinitrc"
                ".xprofile"
                ".emacs.d"))
          (directories '("../../../dotfiles"))))
    ))
    ;; `(( ".xinitrc" ,(plain-file "xinitrc"
    ;;   (string-append
    ;;     "xrdb -merge ~/.Xresources\n"
    ;;     "pcmanfm --desktop &\n"
    ;;     "tint2 &\n"
    ;;     "exec openbox\n")))
    ;;   (".Xresources" ,(plain-file "xresources"
    ;;     (string-append
    ;;       "Xft.dpi: 96\n"
    ;;       "Xft.autohint: 0\n"
    ;;       "Xft.lcdfilter: lcddefault\n"
    ;;       "Xft.hintstyle: hintfull\n"
    ;;       "Xft.hinting: 1\n"
    ;;       "Xft.antialias: 1\n"
    ;;       "Xft.rgba: rgb\n"))))))

(define home-guldan-dotfiles-service-type
  (service-type (name 'home-dotfiles)
                (description "Dotfiles configuration")
                (extensions (list (service-extension home-files-service-type
                                                     home-guldan-dotfiles-configuration)))
                (default-value #f)))

(define home-guldan-environment-variables
  (append (remove (lambda (var)
                    (member (car var)
                            (list "XINITRC" "BROWSER")))
                  home-default-environment-variables)
          (list '("GDK_SCALE" . "2")
                '("GDK_DPI_SCALE" . "1")
                '("QT_SCALE_FACTOR" . "2")
                '("QT_AUTO_SCREEN_SCALE_FACTOR" . "0")
                '("XINITRC" . ".xinitrc"))))

(define guldan-packages
  (list
    ;; Xorg
    util-linux
    xdg-utils
    xdg-desktop-portal
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
    xwallpaper
    xcompmgr
    font-google-noto
    font-google-noto-emoji
    font-adobe-source-code-pro
    adwaita-icon-theme
    hicolor-icon-theme

    ;; Window Manager
    openbox
    obconf
    tint2
    zenity
    pantheon-wallpapers

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
    lf
    ncurses
    ripgrep
    zip
    7zip
    moreutils
    unzip
    htop
    rsync
    pulsemixer
    dosfstools
    curl
    mediainfo
    forgoty-shellutils
    forgoty-st
    luke-dmenu

    ;; Other
    v4l-utils

    ;; Gaming
    steam-nvidia
    heroic-nvidia

    ;; Apps
    flatpak
    nv-codec-headers
    mpv-nvidia))

(define-public guldan-home
  (home-environment
    (services (append %base-home-services
                      (list
                        ;; Pipewire
                        (service home-pipewire-service-type)

                        ;; Shell
                        (service home-zsh-service-type)

                        ;; Dotfiles
                        (service home-guldan-dotfiles-service-type)

                        ;; Set up desktop environment
                        (service home-desktop-service-type
                                  (home-desktop-configuration
                                    (environment-variables home-guldan-environment-variables)
                                    (profile-packages guldan-packages)
                                    (shepherd-services '())))

                        (service home-dbus-service-type)

                        (service home-startx-command-service-type
                                 (xorg-configuration
                                   (keyboard-layout default-keyboard-layout)
                                   (modules (cons nvda %default-xorg-modules))
                                   (resolutions '((1920 1080)))
                                   (drivers '("nvidia"))))

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
                                (respawn? #t))))))))

guldan-home
