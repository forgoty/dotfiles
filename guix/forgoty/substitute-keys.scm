(define-module (forgoty substitute-keys)
  #:use-module (guix gexp))

(define-public nonguix.pub
  (plain-file "nonguix.pub"
    "(public-key
       (ecc
         (curve Ed25519)
         (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))

(define-public guldan.pub
  (plain-file "guldan.pub"
    "(public-key
       (ecc
         (curve Ed25519)
         (q #79E27701F3E1370890C39B3E89E4F9E03D500CA840C689435313648888E6398F#)))"))
