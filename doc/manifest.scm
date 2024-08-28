(use-modules (gnu packages texinfo)
             (guix profiles))

(packages->manifest
 (list texinfo texi2html))
