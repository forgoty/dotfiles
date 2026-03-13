(list (channel
        (name 'guix)
        (url "https://codeberg.org/guix/guix.git")
        (branch "master")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
      (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (introduction
          (make-channel-introduction
            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
            (openpgp-fingerprint
             "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      (channel
        (name 'atomized)
        (url "https://codeberg.org/ieure/atomized-guix.git")
        (branch "main")
        (introduction
          (make-channel-introduction
            "bdbcd3c5815f64799e2c0d139896da83d9972bd1"
          (openpgp-fingerprint
            "6980 A9B9 5202 AA11 EB1D  8922 8499 AC88 F1A7 1CF2")))))
