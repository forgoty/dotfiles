(use-service-modules networking ssh)
(use-modules (forgoty systems base-system)
             ((forgoty ssh-keys) #:prefix ssh-keys:))

(list (machine
       (operating-system (load "./guldan.scm"))
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "guldan.local")
                       (system "x86_64-linux")
                       (host-key ssh-keys:guldan.pub)
                       (port 2222)
                       (user %default-username)))))
