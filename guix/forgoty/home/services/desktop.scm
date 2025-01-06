(define-module (forgoty home services desktop)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu packages)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages calcurse)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages file)
  #:use-module (gnu packages telegram)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages moreutils)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages dictionaries)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages man)
  #:use-module (gnu packages syndication)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages spreadsheet)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages version-control)
  #:use-module (forgoty packages suckless)
  #:export (home-desktop-service-type))

(define (home-desktop-configuration config)
  (list
   ;; Xorg and Desktop environment
   xdg-utils
   libnotify
   setxkbmap
   xinit
   xset
   xmodmap
   xrandr
   xrdb
   xwallpaper
   xprop
   xwininfo
   xbacklight
   unclutter-xfixes
   xdotool
   xclip
   xcompmgr
   dunst
   slock
   font-adobe-source-han-sans
   font-google-noto
   font-google-noto-emoji
   forgoty-st
   forgoty-dwm
   forgoty-dwmblocks
   luke-dmenu

   ;; Shell and CLI tools
   zsh-syntax-highlighting
   zsh-completions
   file
   bat
   neovim
   lf
   git
   wget
   bc
   fzf
   maim
   ripgrep
   git-delta
   ffmpeg
   zip
   socat
   moreutils
   unzip
   htop
   translate-shell
   atool
   calcurse
   rsync
   tesseract-ocr
   sc-im
   newsboat
   wireplumber
   redshift
   blueman
   nsxiv
   poppler
   dosfstools
   pulsemixer
   yt-dlp
   curl
   transmission
   python-ueberzug
   python-qdarkstyle
   man-db
   mediainfo
   polkit
   ncurses

   ;; Apps
   qutebrowser
   telegram-desktop
   zathura
   zathura-pdf-mupdf
   mpv))

(define (home-desktop-environment-variables config)
  '(("GUIX_CONFIG_PATH" . "$HOME/guix-config")
    ("ZSH_DIR" . "$HOME/.guix-home/profile/share/zsh")
    ("BASH_COMPLETIONS_DIR" . "$HOME/.guix-home/profile/etc/bash_completion.d")))

(define home-desktop-service-type
  (service-type (name 'home-desktop)
                (description "Desktop environment configuration")
                (extensions (list (service-extension home-profile-service-type
                                   home-desktop-configuration)
                                  (service-extension
                                   home-environment-variables-service-type
                                   home-desktop-environment-variables)))
                (default-value #f)))
