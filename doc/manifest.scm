(use-modules (gnu packages haskell-xyz)
             (gnu packages python)
             (gnu packages python-xyz)
             (gnu packages texinfo)
             (guix profiles))

(packages->manifest
 (list texinfo texi2html pandoc python python-beautifulsoup4))
