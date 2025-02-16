(define-module (forgoty home services containers)
  #:use-module (gnu packages containers)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:export (podman-service-type))

(define (podman-xdg-configuration-files config)
  `(("containers/registries.conf"
     ,(plain-file
       "registries.conf"
       "unqualified-search-registries = ['docker.io', \
'registry.fedoraproject.org', \
'registry.access.redhat.com', \
'registry.centos.org']"))
    ("containers/storage.conf"
     ,(plain-file
       "storage.conf"
       "[storage]\ndriver = \"btrfs\""))
    ("containers/policy.json"
     ,(plain-file
       "policy.json"
       "{\"default\": [{\"type\": \"insecureAcceptAnything\"}]}"))))

(define podman-service-type
  (service-type (name 'podman-service)
                (description "Installs Podman with some tweaks")
                (extensions (list (service-extension home-profile-service-type
                                                     (lambda (c)
                                                       (list podman podman-compose)))
                                  (service-extension home-xdg-configuration-files-service-type
                                                     podman-xdg-configuration-files)))
                (default-value #f)))
