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

    ;; Window Manager
    openbox
    obconf
    tint2
    hicolor-icon-theme
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
    forgoty-shellutils

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
