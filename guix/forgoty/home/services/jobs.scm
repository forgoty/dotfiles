(define-module (forgoty home services jobs)
  #:use-module (gnu)
  #:use-module (gnu services mcron)
  #:use-module (forgoty packages version-control))

(define-public cerebrum-sync-job
  #~(job '(next-hour)
         (lambda ()
           (chdir (getenv "CEREBRUM_PATH"))
           (system* #$(file-append git-sync "/bin/git-sync") "check")
           (system* #$(file-append git-sync "/bin/git-sync") "sync"))
         "git-sync-cerebrum"
         #:user "nikita"))
