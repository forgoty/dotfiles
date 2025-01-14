(define-module (forgoty home common-desktop)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services shells)
  #:use-module (guix gexp)
  #:use-module (forgoty home services desktop)
  #:use-module (forgoty home services emacs)
  #:use-module (forgoty home services dotfiles)
  #:export (common-desktop-home-services))

(define common-desktop-home-services
  (list
   ;; Pipewire
   (service home-pipewire-service-type)

   ;; OpenSSH service (used mainly for git push)
   (service home-openssh-service-type
	    (home-openssh-configuration
	      (hosts
		(list (openssh-host (name "github.com")
				    (identity-file "~/.ssh/github_forgoty"))))
	      (add-keys-to-agent "yes")))

   ;; Run user dbus session
   (service home-dbus-service-type)

   ;; Zsh
   (service home-zsh-service-type)

   ;; Dotfiles
   (service home-forgoty-dotfiles-service-type)

   ;; Set up desktop environment
   (service home-desktop-service-type)

   ;; Emacs configuration
   (service home-emacs-config-service-type)))
