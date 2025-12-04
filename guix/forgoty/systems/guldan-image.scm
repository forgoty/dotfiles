(use-modules (gnu)
             (gnu system)
             (gnu services admin)
             (gnu services guix)
             (srfi srfi-1)
             (ice-9 textual-ports))

(define guldan (load "./guldan.scm"))

(define root-fs
  (first (filter (lambda (fs)
                   (string=? "/" (file-system-mount-point fs)))
                 (operating-system-file-systems guldan))))

(operating-system
  (inherit guldan)
  (services
   (cons
    (service resize-file-system-service-type
             (resize-file-system-configuration
               (file-system root-fs)))
    (operating-system-user-services guldan))))
