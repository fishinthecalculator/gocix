# gocix

 🌿 Welcome to gocix! This project aims at providing a community managed library of Guix services. Code from this channel implements a Guix native experience for services that are not yet guixable, through [OCI backed Shepherd Services](https://guix.gnu.org/en/manual/devel/en/guix.html#index-oci_002dcontainer_002dservice_002dtype).

## Services

This channel exposes at `(oci services)` a set of Guix System services for many common applications, such as:

- Grafana
- Prometheus
- Matrix Conduit

These services are supposed to feel like services backed by native Guix packages, please report any inconsistency you may find.

## What is a Guix channel?

A [channel](https://guix.gnu.org/en/manual/devel/en/guix.html#Channels) is roughly the Guix equivalent of Ubuntu's PPAs or container registries. It's a software repository providing Guix package and service definitions.

You can search for package and service definitions from this channel any many others at [toys.whereis.みんな](https://toys.whereis.xn--q9jyb4c).

### Configure

To configure Guix for using this channel you need to create a `.config/guix/channels.scm` file with the following content:

``` scheme
(cons* (channel
        (name 'gocix)
        (url "https://git.sr.ht/~fishinthecalculator/gocix")
        (branch "main")
        ;; Enable signature verification:
        (introduction
         (make-channel-introduction
          "cdb78996334c4f63304ecce224e95bb96bfd4c7d"
          (openpgp-fingerprint
           "8D10 60B9 6BB8 292E 829B  7249 AED4 1CC1 93B7 01E2"))))
       %default-channels)
```

Otherwise, if you already have a `.config/guix/channels.scm` you can simply prepend this channel to the preexisting ones:

``` scheme
(cons* (channel
        (name 'gocix)
        (url "https://git.sr.ht/~fishinthecalculator/gocix")
        (branch "main")
        ;; Enable signature verification:
        (introduction
         (make-channel-introduction
          "cdb78996334c4f63304ecce224e95bb96bfd4c7d"
          (openpgp-fingerprint
           "8D10 60B9 6BB8 292E 829B  7249 AED4 1CC1 93B7 01E2"))))
       (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        ;; Enable signature verification:
        (introduction
         (make-channel-introduction
          "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
          (openpgp-fingerprint
           "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
       %default-channels)
```

## Contributing

All contributions are welcome. To contribute send your patches to [~fishinthecalculator/public-inbox@lists.sr.ht](https://lists.sr.ht/~fishinthecalculator/public-inbox).

If you have commit access please remember to setup the authentication hook with

```bash
cp -v etc/git/pre-push .git/hooks/pre-push
```

## License

Unless otherwise stated all the files in this repository are to be considered under the GPL 3.0 terms. You are more than welcome to open issues or send patches.
