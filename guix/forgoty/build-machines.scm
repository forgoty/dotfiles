(define-module (forgoty build-machines)
  #:use-module (guix gexp)
  #:use-module ((forgoty ssh-keys) #:prefix ssh-keys:))

(define-public guldan-build-machine
  #~(build-machine
      (name "guldan.local")
      (systems (list "x86_64-linux"))
      (user "nikita")
      (host-key (ungexp ssh-keys:guldan.pub))
      (port 2222)
      (private-key "/home/nikita/.ssh/guldan")
      (speed 2.0)))
