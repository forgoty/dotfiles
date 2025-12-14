(define-module (forgoty packages linux)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages linux)
  #:use-module (guix git-download))

(define-public evdi-linux
  ;; You should install this into system packages. Also to make it work
  ;; some adjusted to system services are needed like below:
  ;; (service kernel-module-loader-service-type '("evdi"))
  ;; (simple-service 'evdi-config etc-service-type
  ;;                 (list `("modprobe.d/evdi.conf"
  ;;                         ,(plain-file "evdi.conf"
  ;;                                      "options evdi initial_device_count=1"))))
  ;; 
  ;; Also need to enable kernel module in "operating-system":
  ;; (kernel-loadable-modules (list evdi-linux))
  (package
    (inherit evdi)
    (name "evdi")
    (version "1.14.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/DisplayLink/evdi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mg9gbzgxwgdcniy9kijsyj0asvmdxd63kh88810igzsxs3185jb"))))
    (arguments
     (list #:tests? #f
           #:linux linux
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'chdir
                          (lambda _
                            (chdir "module")))
                        (add-after 'unpack 'fix-os-release
                          (lambda _
                            (define (touch file)
                              (call-with-output-file file
                                (const #t)))
                            (let* ((hard-path "/etc/os-release")
                                    (fixed-path (string-append #$output hard-path)))
                              ;; Make it relative
                              ;; Update hardcoded path to something
                              ;; within the build enviroment.
                              (substitute* "module/Makefile"
                                ((hard-path)
                                  fixed-path))
                              ;; Create directory for the dummy file.
                              (mkdir-p (string-append #$output "/etc"))
                              (touch fixed-path)))))))))

