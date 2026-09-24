(define-module (forgoty packages firmware)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix guix-license:)
  #:use-module (nonguix licenses))


(define-public mt7927-bt-firmware
  (let ((commit "92507536fbbedaaa1e20e103ad67a502a7a50b0b")
        (revision "1"))
    (package
      (name "mt7927-bt-firmware")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/morrownr/mt76")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "01xglmjf8pmf5nx7wrqild674giyfyh2mnpwcxr797fa3dwqqlfq"))))
      (build-system copy-build-system)
      (arguments
       (list
        #:substitutable? #f
        #:install-plan
        #~'(("firmware/mt7927/BT_RAM_CODE_MT6639_2_1_hdr.bin"
             "/lib/firmware/mediatek/mt7927/"))))
      (synopsis "Bluetooth firmware for MediaTek MT7927")
      (description
       "This package provides the Bluetooth firmware for the MediaTek
MT6639 Bluetooth controller used by MT7927/Filogic 380 devices.")
      (home-page "https://github.com/morrownr/mt76")
      (license (nonfree (string-append "unknown"))))))
