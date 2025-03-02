(define-module (forgoty home services shellutils)
  #:use-module (gnu home services)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (forgoty-direnv-service-type))

(define (forgoty-direnv-config config)
`(("direnv/direnvrc" ,(plain-file
                             "direnvrc"
                             "\
use_guixs() {
  LOCK_FILE=channels-lock.scm
  if [ -f $LOCK_FILE ]; then
    eval \"$(guix time-machine -C $LOCK_FILE -- shell \"$@\" --search-paths)\"
  else
    eval \"$(guix shell \"$@\" --search-paths)\"
  fi
}"))))

(define forgoty-direnv-service-type
  (service-type (name 'forgoty-direnv-service)
                (description "forgoty-direnv")
                (extensions (list (service-extension home-profile-service-type
                                                     (lambda (c)
                                                       (list direnv)))
                                  (service-extension home-xdg-configuration-files-service-type
                                                     forgoty-direnv-config)))
                (default-value #f)))
