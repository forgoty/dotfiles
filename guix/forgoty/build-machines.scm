(define-module (forgoty build-machines)
  #:use-module (guix gexp)
  #:use-module ((forgoty systems base-system) #:select (%default-username))
  #:use-module ((forgoty ssh-keys) #:prefix ssh-keys:))

(define-public guldan-build-machine
  #~(build-machine
      (name "192.168.100.100")
      (systems (list "x86_64-linux"))
      (user %default-username)
      (host-key (ungexp ssh-keys:guldan.pub))
      (port 2222)
      (private-key (format #f "/home/~a/.ssh/guldan" %default-username))
      (speed 2.0)))
