;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (gnu home)
             (gnu packages)
             (gnu home services shells)
             (gnu home services sound)
             (gnu home services dotfiles)
             (gnu home services desktop)
             (guix gexp)
             (gnu packages rsync)
             (gnu packages web-browsers)
             (gnu packages shellutils)
             (gnu packages xorg)
             (gnu packages wm)
             (gnu packages gnome)
             (gnu packages calcurse)
             (gnu packages image-viewers)
             (gnu packages tree-sitter)
             (gnu packages python-xyz)
             (gnu packages algebra)
             (gnu packages polkit)
             (gnu packages fonts)
             (gnu packages wget)
             (gnu packages file)
             (gnu packages telegram)
             (gnu packages rust-apps)
             (gnu packages curl)
             (gnu packages ncurses)
             (gnu packages networking)
             (gnu packages bittorrent)
             (gnu packages moreutils)
             (gnu packages suckless)
             (gnu packages admin)
             (gnu packages ocr)
             (gnu packages dictionaries)
             (gnu packages pdf)
             (gnu packages man)
             (gnu packages syndication)
             (gnu packages linux)
             (gnu packages pulseaudio)
             (gnu packages spreadsheet)
             (gnu packages terminals)
             (gnu packages video)
             (gnu packages xdisorg)
             (gnu packages compression)
             (gnu packages freedesktop)
             (gnu packages disk)
             (gnu packages vim)
             (gnu packages version-control)
             (gnu home services)
             (gnu services)
             (forgoty packages suckless)
             (forgoty home services emacs))

(home-environment
  ;; Below is the list of packages that will show up in your
  ;; Home profile, under ~/.guix-home/profile.
  (packages (list
             ;; Xorg and Window Management
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
             xclip
             unclutter-xfixes
             xdotool
             xcompmgr
             dunst
             slock
             ;; Forgoty
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
             calcurse
             maim
             ripgrep
             git-delta
             zip
             socat
             moreutils
             unzip
             htop
             tesseract-ocr
             translate-shell
             atool
             rsync
             sc-im
             newsboat
             yt-dlp
             curl
             transmission

             ;; Apps and Tools
             qutebrowser
             zathura
             zathura-pdf-mupdf
             redshift
             mpv
             nsxiv
             ffmpeg
             telegram-desktop

             ;; Support
             python-ueberzug
             python-qdarkstyle
             man-db
             pulsemixer
             poppler
             mediainfo
             polkit
             blueman
             font-adobe-source-han-sans
             font-google-noto
             font-google-noto-emoji
             tree-sitter
             ncurses
             dosfstools
             wireplumber))

  ;; Below is the list of Home services.  To search for available
  ;; services, run 'guix home search KEYWORD' in a terminal.
  (services
   (list (service home-emacs-config-service-type)
         (service home-zsh-service-type
                  (home-zsh-configuration (environment-variables '(("GUIX_CONFIG_PATH" . "$HOME/guix-config")
                                                                   ("GUIX_PACKAGE_PATH" . "$GUIX_CONFIG_PATH/config/packages")
                                                                   ("GUILE_LOAD_PATH" . "$GUILE_LOAD_PATH:$GUIX_PACKAGE_PATH")
                                                                   ("ZSH_DIR" . "$HOME/.guix-home/profile/share/zsh")
                                                                   ("BASH_COMPLETIONS_DIR" . "$HOME/.guix-home/profile/etc/bash_completion.d")))))
         (simple-service 'custom/home-shell-profile-service-type
                         home-shell-profile-service-type
                         `(,(local-file
                             "../../../dotfiles/.config/shell/profile")))
         (service home-dbus-service-type)
         (service home-pipewire-service-type)
         (service home-dotfiles-service-type
                  (home-dotfiles-configuration (excluded '("README.md"
                                                           ".stow-local-ignore"))
                                               (directories '("../../../dotfiles")))))))
