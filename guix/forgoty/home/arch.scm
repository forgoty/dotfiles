(define-module (forgoty home arch)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages file)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages video)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages moreutils)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages dictionaries)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages spreadsheet)
  #:use-module (gnu packages syndication)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages web)
  #:use-module (gnu packages man)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages linux)
  #:use-module (forgoty packages suckless)
  #:use-module (forgoty packages ocr)
  #:use-module (forgoty packages python-xyz)
  #:use-module ((srfi srfi-1) #:hide (zip))
)

(define (home-arch-desktop-packages config)
  (list
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
    rsync
    tesseract-ocr
    (make-tesseract-ocr-language-package "pol" "0d1nj5f4hgbkc1c9mgda1imppslavxbr91w4k6kwlp469ryagd8h")
    (make-tesseract-ocr-language-package "rus" "0h4cw4zl7r1dj6yxhfzcia15xad707pbbi779ssn2xj1pjmvgfgx")
    sc-im
    newsboat
    ;;wireplumber
    ;;redshift
    ;;blueman
    nsxiv
    poppler
    dosfstools
    pulsemixer
    yt-dlp
    jq
    curl
    transmission
    ueberzug-bin
    man-db
    mediainfo
    polkit
    ncurses
    bmon
    flatpak
    strace))

(define arch-desktop-service-type
  (service-type (name 'arch-desktop)
                (description "arch desktop service")
                (extensions (list (service-extension home-profile-service-type
                                                     home-arch-desktop-packages)))
                (default-value #f)))

(define home-arch-desktop-services
  (append (list
           (service arch-desktop-service-type))))

(home-environment
  (services home-arch-desktop-services))
