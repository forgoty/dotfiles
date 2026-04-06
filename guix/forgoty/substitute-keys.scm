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

(define-public guix-science.pub
  (plain-file "guix-science.pub"
    "(public-key
       (ecc
         (curve Ed25519)
         (q #89FBA276A976A8DE2A69774771A92C8C879E0F24614AAAAE23119608707B3F06#)))"))

(define-public cache-cdn.guix.moe.pub
  (plain-file "cache-cdn.guix.moe.pub"
    "(public-key
      (ecc
        (curve Ed25519)
        (q #552F670D5005D7EB6ACF05284A1066E52156B51D75DE3EBD3030CD046675D543#)))"))
