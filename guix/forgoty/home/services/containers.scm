(define-module (forgoty home services containers)
  #:use-module (gnu packages containers)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services)
  #:export (podman-service-type
            podman-compose-service-type
            podman-compose-configuration))

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

(define-record-type* <podman-compose-configuration>
  podman-compose-configuration make-podman-compose-configuration
  podman-compose-configuration?
  (podman-compose podman-compose-configuration-podman-compose
                  (default podman-compose))
  (compose-file podman-compose-configuration-compose-file)
  (project-name podman-compose-configuration-project-name)
  (requirement podman-compose-configuration-requirement
               (default '()))
  (respawn? podman-compose-configuration-respawn?
            (default #f)))

(define (podman-compose-activation config)
  #~(begin
      (use-modules ((shepherd support):hide (mkdir-p)))
      (mkdir-p (string-append %user-log-dir "/podman-compose/"))))

(define (podman-compose-shepherd-service config)
  (match-record config <podman-compose-configuration>
    (podman-compose compose-file project-name requirement respawn?)
    (let* ((command #~(list (string-append #$podman-compose "/bin/podman-compose") 
                       "--file" #$compose-file
                       "--project-name" #$project-name
                       "up"))
           (log-file #~(string-append %user-log-dir "/podman-compose/" #$project-name ".log")))
     (list
      (shepherd-service
       (provision (list (string->symbol (string-append "podman-compose-" project-name))))
       (documentation (string-append "Run podman-compose-" project-name))
       (requirement requirement)
       (modules '((shepherd support)))
       (start #~(make-forkexec-constructor #$command
                 #:log-file #$log-file))
       (respawn? respawn?)
       (stop #~(make-kill-destructor)))))))

(define podman-compose-service-type
  (service-type (name 'podman-compose)
                (default-value #f)
                (extensions (list (service-extension home-activation-service-type
                                                     podman-compose-activation)
                                  (service-extension home-shepherd-service-type
                                                     podman-compose-shepherd-service)))
                (description "Run podman compose service")))
