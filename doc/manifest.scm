(use-modules (gnu packages python)
             (gnu packages python-xyz)
             (gnu packages texinfo)
             (guix profiles))

(packages->manifest
 (list texinfo texi2html python python-beautifulsoup4))
