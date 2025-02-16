(define-module (forgoty home common-desktop)
  #:use-module (gnu services)
  #:use-module (gnu home)
  #:use-module (gnu)
  #:use-module (gnu services xorg)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services shells)
  #:use-module (guix gexp)
  #:use-module (forgoty home services desktop)
  #:use-module (forgoty home services emacs)
  #:use-module (forgoty home services dotfiles)
  #:use-module (forgoty home services containers)
  #:use-module (forgoty systems base-system)
  #:export (common-desktop-home-services))

(define common-desktop-home-services
  (append (list
           ;; Pipewire
           (service home-pipewire-service-type)

           ;; Add startx to path
           (service home-startx-command-service-type
                   (xorg-configuration
                     (keyboard-layout default-keyboard-layout)))

           ;; Run user dbus session
           (service home-dbus-service-type)

           ;; Zsh
           (service home-zsh-service-type)

           ;; Dotfiles
           (service home-forgoty-dotfiles-service-type)

           ;; Set up desktop environment
           (service home-desktop-service-type)

           ;; OpenSSH service (used mainly for git push)
           (service home-openssh-service-type
             (home-openssh-configuration
               (hosts
           (list (openssh-host (name "github.com")
                   (identity-file "~/.ssh/github_forgoty"))))
               (add-keys-to-agent "yes")))

           ;; Emacs configuration
           (service home-emacs-config-service-type)

           ;; Podman
           (service podman-service-type))
          %base-home-services))
