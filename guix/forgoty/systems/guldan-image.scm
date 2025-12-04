(define-module (forgoty systems guldan-image)
  #:use-module (gnu)
  #:use-module (gnu image)
  #:use-module (gnu system image)
  #:use-module (gnu services admin)
  #:use-module (gnu services base)
  #:use-module ((srfi srfi-1) #:select (first filter))
  #:use-module (forgoty systems guldan))

(define root-fs
  (first (filter (lambda (fs)
                   (string=? "/" (file-system-mount-point fs)))
                 (operating-system-file-systems guldan))))

(define os-with-resize
  (operating-system
    (inherit guldan)
    (services
     (cons
      (service resize-file-system-service-type
                      (resize-file-system-configuration
                        (file-system root-fs)))
      (operating-system-user-services guldan)))))

(define guldan-image
  (image
    (format 'disk-image)
    (partition-table-type 'gpt)
    (operating-system os-with-resize)
    (partitions
      (list
        esp-partition/grub
        (partition
          (size 'guess)
          (label root-label)
          (file-system "btrfs")
          (flags '(boot))
          (initializer (gexp initialize-root-partition)))))))

guldan-image
