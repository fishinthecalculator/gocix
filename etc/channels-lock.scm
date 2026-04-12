(list (channel
       (name 'guix)
       (url "https://codeberg.org/fishinthecalculator/guix-mirror")
       (branch "oci-fetch")
       (commit "8866f2950b5ac7feae1fc49c05a45d0c6ce699d2")
       (introduction
        (make-channel-introduction
         "9edb3f66fd807b096b48283debdcddccfea34bad"
         (openpgp-fingerprint
          "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
      (channel
       (name 'bluebox)
       (url "https://codeberg.org/lapislazuli/bluebox")
       (branch "main")
       (commit "f7f3b1565d626099dd68aae20abb36371ab8a9af")
       (introduction
        (make-channel-introduction
         "63350484aaacc362aea28fb14236019fced4050f"
         (openpgp-fingerprint
          "5132 3571 CEED 988F 52FC  467C 6F98 DBF3 EA7F 4B37"))))
      (channel
       (name 'sops-guix)
       (url "https://github.com/fishinthecalculator/sops-guix.git")
       (branch "main")
       (commit "7d3eb6069c71fa3f4281d6a0b209e55cb96a8674")
       (introduction
        (make-channel-introduction
         "0bbaf1fdd25266c7df790f65640aaa01e6d2dbc9"
         (openpgp-fingerprint
          "8D10 60B9 6BB8 292E 829B  7249 AED4 1CC1 93B7 01E2")))))
