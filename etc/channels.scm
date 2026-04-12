(list (channel
       (name 'guix)
       (url "https://codeberg.org/fishinthecalculator/guix-mirror")
       (branch "oci-fetch")
       (introduction
        (make-channel-introduction
         "9edb3f66fd807b096b48283debdcddccfea34bad"
         (openpgp-fingerprint
          "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
      (channel
       (name 'bluebox)
       (url "https://codeberg.org/lapislazuli/bluebox")
       (branch "main")
       (introduction
        (make-channel-introduction
         "63350484aaacc362aea28fb14236019fced4050f"
         (openpgp-fingerprint
          "5132 3571 CEED 988F 52FC  467C 6F98 DBF3 EA7F 4B37"))))
      (channel
       (name 'gocix-git)
       (url (string-append "file://" (canonicalize-path (getcwd))))
       (branch "system_tests")
       (introduction
        (make-channel-introduction
         "cdb78996334c4f63304ecce224e95bb96bfd4c7d"
         (openpgp-fingerprint
          "8D10 60B9 6BB8 292E 829B  7249 AED4 1CC1 93B7 01E2")))))
