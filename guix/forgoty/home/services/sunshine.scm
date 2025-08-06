(define-module (forgoty home services sunshine)
  #:use-module (srfi srfi-26)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (forgoty packages streaming)
  #:export (home-sunshine-service-type
            home-sunshine-configuration))

(define-record-type* <home-sunshine-configuration>
  home-sunshine-configuration make-home-sunshine-configuration
  home-sunshine-configuration?
  (sunshine          home-sunshine-sunshine
                 (default sunshine))
  (config-file-path home-sunshine-config-file-path
                 (default (string-append "~/.config/sunshine/sunshine.conf"))))

(define (home-sunshine-services config)
  "Return a <shepherd-service> for sunshine with CONFIG."
  (match-record config <home-sunshine-configuration>
    (sunshine config-file-path)
    (let* ((sunshine (file-append sunshine "/bin/sunshine"))
           (command #~(list #$sunshine #$config-file-path))
           (log-file #~(string-append %user-log-dir "/sunshine.log")))
      (list (shepherd-service
             (documentation "Run the sunshine host.")
             (provision '(sunshine))
             (modules '((shepherd support)))
             (start #~(make-forkexec-constructor #$command
                                                 #:log-file #$log-file))
             (stop #~(make-kill-destructor)))))))

(define home-sunshine-service-type
  (service-type
   (name 'home-sunshine)
   (default-value (home-sunshine-configuration))
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-sunshine-services)))
   (description "Run the sunshine streaming server")))

