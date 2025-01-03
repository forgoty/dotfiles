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
             (gnu packages emacs)
             (gnu packages emacs-xyz)
	     (forgoty packages suckless))

(home-environment
  ;; Below is the list of packages that will show up in your
  ;; Home profile, under ~/.guix-home/profile.
  (packages (list
		  ; Xorg and Window Management
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

		  ; Shell and CLI tools
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

		  ; Apps and Tools
		  qutebrowser
		  zathura
		  zathura-pdf-mupdf
		  redshift
		  mpv
		  nsxiv
		  ffmpeg
		  telegram-desktop

		  ; Support
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
		  wireplumber

		  ; Emacs
		  emacs

		  ;; Tools
		  emacs-flycheck
		  emacs-restart-emacs
		  emacs-google-c-style

		  ;; Search and Completion
		  emacs-magit
		  emacs-cape
		  emacs-consult
		  emacs-corfu
		  emacs-corfu-terminal
		  emacs-embark
		  emacs-marginalia
		  emacs-orderless
		  emacs-vertico

		  ;; Evil
		  emacs-evil
		  emacs-evil-collection
		  emacs-evil-nerd-commenter
		  emacs-evil-anzu
		  emacs-evil-surround
		  emacs-evil-visualstar

		  ;; Input
		  emacs-general
		  emacs-which-key

		  ;; IDE
		  emacs-editorconfig
		  emacs-aggressive-indent
		  emacs-package-lint
		  emacs-zig-mode

		  ;; Org
		  emacs-org-super-agenda

		  ;; UI
		  emacs-nerd-icons
		  emacs-golden-ratio
		  emacs-elisp-demos
		  emacs-helpful
		  emacs-doom-themes
		  emacs-doom-modeline
		  emacs-popwin

		  ;; Writing
		  emacs-markdown-mode
		  emacs-pandoc-mode
		  emacs-auctex))

  ;; Below is the list of Home services.  To search for available
  ;; services, run 'guix home search KEYWORD' in a terminal.
  (services
   (list
     (service home-zsh-service-type)
     (service home-dbus-service-type)
     (service home-pipewire-service-type)
     (service home-dotfiles-service-type
	      (home-dotfiles-configuration
		(excluded '("README.md" ".stow-local-ignore" ".zprofile"))
		(directories '("../../files/dotfiles"))))
     (simple-service 'some-useful-env-vars-service
	home-environment-variables-service-type
	`(("EDITOR" . "nvim")
	  ("TERMINAL" . "st")
	  ("TERMINAL_PROG" . "st")
	  ("BROWSER" . "qutebrowser")
	  ("XDG_CONFIG_HOME" . "$HOME/.config")
	  ("XDG_DATA_HOME" . "$HOME/.local/share")
	  ("XDG_CACHE_HOME" . "$HOME/.cache")
	  ("XINITRC" . "$XDG_CONFIG_HOME/x11/xinitrc")
	  ("GTK2_RC_FILES" . "$XDG_CONFIG_HOME/gtk-2.0/gtkrc-2.0")
	  ("WGETRC" . "$XDG_CONFIG_HOME/wget/wgetrc")
	  ("INPUTRC" . "$XDG_CONFIG_HOME/shell/inputrc")
	  ("ZDOTDIR" . "$XDG_CONFIG_HOME/zsh")
	  ("WINEPREFIX" . "$XDG_DATA_HOME/wineprefixes/default")
	  ("PASSWORD_STORE_DIR" . "$XDG_DATA_HOME/password-store")
	  ("ANDROID_SDK_HOME" . "$XDG_CONFIG_HOME/android")
	  ("CARGO_HOME" . "$XDG_DATA_HOME/cargo")
	  ("GOPATH" . "$XDG_DATA_HOME/go")
	  ("GUIX_PACKAGE_PATH" . "$HOME/guix-config/config/packages")
	  ("GUILE_LOAD_PATH" . "$GUILE_LOAD_PATH:$(echo $GUIX_PACKAGE_PATH)")
	  ("GOMODCACHE" . "$XDG_CACHE_HOME/go/mod")
	  ("ZSH_DIR" . "$HOME/.guix-home/profile/share/zsh")
	  ("BASH_COMPLETIONS_DIR" . "$HOME/.guix-home/profile/etc/bash_completion.d")
	  ("ANSIBLE_CONFIG" . "$XDG_CONFIG_HOME/ansible/ansible.cfg")
	  ("UNISON" . "$XDG_DATA_HOME/unison")
	  ("HISTFILE" . "$XDG_DATA_HOME/history")
	  ("MBSYNCRC" . "$XDG_CONFIG_HOME/mbsync/config")
	  ("ELECTRUMDIR" . "$XDG_DATA_HOME/electrum")
	  ("PYTHONSTARTUP" . "$XDG_CONFIG_HOME/python/pythonrc")
	  ("SQLITE_HISTORY" . "$XDG_DATA_HOME/sqlite_history")
	  ("DICS" . "/usr/share/stardict/dic/")
	  ("SUDO_ASKPASS" . "$HOME/.local/bin/dmenupass")
	  ("FZF_DEFAULT_OPTS" . "--layout=reverse --height 40%")
	  ("LESS" . "R")
	  ("LESS_TERMCAP_mb" . "$(printf '%b' '[1;31m')")
	  ("LESS_TERMCAP_md" . "$(printf '%b' '[1;36m')")
	  ("LESS_TERMCAP_me" . "$(printf '%b' '[0m')")
	  ("LESS_TERMCAP_so" . "$(printf '%b' '[01;44;33m')")
	  ("LESS_TERMCAP_se" . "$(printf '%b' '[0m')")
	  ("LESS_TERMCAP_us" . "$(printf '%b' '[1;32m')")
	  ("LESS_TERMCAP_ue" . "$(printf '%b' '[0m')")
	  ("QT_QPA_PLATFORMTHEME" . "gtk2") ; Have QT use gtk2 theme.
	  ("MOZ_USE_XINPUT2" . #t) ; Mozilla smooth scrolling/touchpads
	  ("AWT_TOOLKIT" . "MToolkit wmname LG3D") ; May have to install wmname
	  ("PATH" . "$PATH:$(echo $GOPATH/bin)")
	  ("PATH" . "$PATH:$(echo $HOME/.local/bin)")
	  ("PATH" . "$PATH:$(echo $HOME/.local/bin/statusbar)")
	  ("_JAVA_AWT_WM_NONREPARENTING" . #t))))))
