(use-service-modules networking ssh)
(use-modules (forgoty systems base-system))

(list (machine
       (operating-system (load "./guldan.scm"))
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "192.168.100.100")
                       (system "x86_64-linux")
                       (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINPCAkL8ejoYwTR+sN3PrcoTpa0M+Wq1hkvK47G/Z4+s")
                       (port 2222)
                       (user %default-username)))))
