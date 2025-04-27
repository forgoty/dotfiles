(define-module (forgoty home services desktop)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shells)
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
  #:use-module (gnu packages mail)
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
  #:use-module (forgoty packages ocr)
  #:export (home-desktop-service-type))

(define (home-desktop-configuration config)
  (list
   ;; Xorg and Desktop environment
   util-linux
   xdg-utils
   libnotify
   setxkbmap
   xset
   xmodmap
   xrandr
   xrdb
   xwallpaper
   xprop
   xwininfo
   lm-sensors
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
   (make-tesseract-ocr-language-package "pol" "0d1nj5f4hgbkc1c9mgda1imppslavxbr91w4k6kwlp469ryagd8h")
   (make-tesseract-ocr-language-package "rus" "0h4cw4zl7r1dj6yxhfzcia15xad707pbbi779ssn2xj1pjmvgfgx")
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
   bmon

   ;; Apps
   qutebrowser
   telegram-desktop
   neomutt
   zathura
   zathura-pdf-mupdf
   mpv))

(define (home-desktop-environment-variables config)
  '(("XINITRC" . "$XDG_CONFIG_HOME/x11/xinitrc")
    ("GTK2_RC_FILES" . "$XDG_CONFIG_HOME/gtk-2.0/gtkrc-2.0")
    ("EDITOR" . "nvim")
    ("TERMINAL" . "st")
    ("BROWSER" . "qutebrowser")
    ("WGETRC" . "$XDG_CONFIG_HOME/wget/wgetrc")
    ("INPUTRC" . "$XDG_CONFIG_HOME/shell/inputrc")
    ("ZDOTDIR" . "$XDG_CONFIG_HOME/zsh")
    ("HISTFILE" . "$XDG_DATA_HOME/history")
    ("CEREBRUM_PATH" . "$HOME/cerebrum")
    ("SUDO_ASKPASS" . "dmenupass")
    ("FZF_DEFAULT_OPTS" . "--layout=reverse --height 40%")
    ("LESS" . "R")
    ("LESSOPEN" . "| /usr/bin/highlight -O ansi %s 2>/dev/null")
    ("QT_QPA_PLATFORMTHEME" . "gtk2")
    ("_JAVA_AWT_WM_NONREPARENTING" . "1")
    ("ZSH_DIR" . "$HOME/.guix-home/profile/share/zsh")
    ("BASH_COMPLETIONS_DIR" . "$HOME/.guix-home/profile/etc/bash_completion.d")))

(define home-desktop-service-type
  (service-type (name 'home-desktop)
                (description "Desktop environment configuration")
                (extensions (list (service-extension home-profile-service-type
                                                     home-desktop-configuration)
                                  (service-extension home-shell-profile-service-type
                                                     (const (list (plain-file
                                                                   "startx"
                                                                   "[ $(tty) = /dev/tty1 ] && exec startx"))))
                                  (service-extension
                                   home-environment-variables-service-type
                                   home-desktop-environment-variables)))
                (default-value #f)))
