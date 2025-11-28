(define-module (forgoty home arch)
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages base)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (gnu home)
  #:use-module (gnu services xorg)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services mcron)
  #:use-module (gnu home services shells)
  #:use-module (forgoty home common-desktop)
  #:use-module (forgoty home services desktop)
  #:use-module (forgoty home services desktop)
  #:use-module (forgoty home services emacs)
  #:use-module (forgoty home services dotfiles)
  #:use-module (forgoty home services containers)
  #:use-module (forgoty home services shellutils)
  #:use-module (forgoty home services jobs)
  #:use-module (forgoty systems base-system)
  #:export (home-arch-packages
            home-arch-environment-variables))

(define home-arch-packages
  (append (remove (lambda (pkg)
            (member pkg
                    (list slock qemu virt-manager util-linux)))
                  home-default-packages)
          (list
            aspell
            aspell-dict-en
            emacs-org-re-reveal
            ;; required for "foreign" distributions for locale support
            glibc-locales)))

(define home-arch-environment-variables
  (append (remove (lambda (var)
                    (member (car var)
                            (list "XLOCK" "SHUTDOWN" "REBOOT" "SUSPEND" "DOCKER")))
                    home-default-environment-variables)
          (list '("SSL_CERT_FILE" . "/etc/ssl/certs/ca-certificates.crt")
                '("XLOCK" . "slock")
                '("SHUTDOWN" . "sudo /usr/bin/shutdown -h now")
                '("REBOOT" . "sudo /usr/bin/reboot")
                '("DOCKER" . "docker")
                '("SUSPEND" . "sudo /usr/bin/systemctl suspend")
		'("CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC". "1")
		'("CLAUDE_CODE_IDE_SKIP_AUTO_INSTALL". "1")
		'("MCP_TIMEOUT". "600000")
		'("USE_BUILTIN_RIPGREP". "0"))))

(define home-arch-desktop-services
  (append (list
           ;; Pipewire
           ;; (service home-pipewire-service-type) ;; pulseaudio uses arch's pipewire package

           ;; Add startx to path
           (service home-startx-command-service-type
                    (xorg-configuration (keyboard-layout default-keyboard-layout)))

           ;; Run user dbus session
           (service home-dbus-service-type)

           ;; Shell
           (service home-zsh-service-type)
           (service forgoty-direnv-service-type)

           ;; Dotfiles
           (service home-forgoty-dotfiles-service-type)

           ;; Set up desktop environment
           (service home-desktop-service-type
                    (home-desktop-configuration
                     (profile-packages home-arch-packages)
                     (environment-variables home-arch-environment-variables)))

           ;; Run unclutter
           (service home-x11-service-type) ;; wait x11 to start
           (service home-unclutter-service-type
                    (home-unclutter-configuration
                     (unclutter unclutter-xfixes)
                     (idle-timeout 2)))

           (service home-ssh-agent-service-type)

           ;; Background cron jobs
           (service home-mcron-service-type
                    (home-mcron-configuration (jobs (list cerebrum-sync-job))))

           ;; Emacs configuration
           (service home-emacs-config-service-type)

           ;; Podman
           (service podman-service-type
                    (podman-configuration
                     (xdg-configuration-files (podman-default-xdg-configuration-files "overlay")))))
          %base-home-services))

(home-environment
  (services home-arch-desktop-services))
