# gocix

 🌿 Welcome to gocix! This project aims at providing a community managed library of Guix services. Code from this channel implements a Guix native experience for services that are not yet guixable, through [OCI backed Shepherd Services](https://guix.gnu.org/manual/devel/en/guix.html#index-oci_002dcontainer_002dservice_002dtype).

The inclusion of the main piece of this project - `oci-service-type` - into Guix is currently being discussed [in an issue](https://issues.guix.gnu.org/76081).

### Migrating to the `oci-service-type`

The `oci-service-type` deprecates the `ci-container-service-type`: it is
completely backward compatible and now, while deprecated, the
`oci-container-service-type` is actually implemented extending the
`oci-service-type`.

It brings additional features, such as: rootless podman support, the ability
to provision networks and volumes, and better image caching.

To make the switch in service code you need to change your extension from
 ``` lisp
(service-extension oci-container-service-type
                   oci-bonfire-configuration->oci-container-configuration)
```
to
 ``` lisp
(service-extension oci-service-type
                   (lambda (config)
                     (oci-extension
                      (containers
                       (list
                        (oci-bonfire-configuration->oci-container-configuration config))))))
```

To make the switch in `operating-system` records, you need to change from

 ``` lisp
(simple-service 'oci-containers
                oci-container-service-type
                (list
                 (oci-container-configuration
                  ...)))
```
to
 ``` lisp
(simple-service 'oci-containers
                oci-service-type
                (oci-extension
                 (containers
                  (list
                   (oci-container-configuration
                    ...)))))
```

## Motivation

Services in gocix are supposed to be used to run useful stuff with Guix. They are for the community by the community and there are all the intentions of collaborating with upstream once underlying packages are into Guix proper. To achieve this vision gocix services strive to:

- Separate as much as possible configurations that pertain to the OCI implementation (i.e. ports, volumes, OCI images tags and so on) from application configurations (i.e. whether the application will use https or whatever else). The idea is to make it as little effort as possible upstreaming code from this channel to Guix.
- Services are configured with sane, secure, privacy aware defaults when possible. This means that each service in this repository is carefully reviewed and analytics are disabled whenever possible.

## Services

This channel exposes at `(oci services)` a set of Guix System services for many useful applications, such as:

- [Bonfire Classic](https://bonfirenetworks.org/app/classic/)
- [Forgejo](https://forgejo.org/)
- [Grafana](https://grafana.com/)
- [Matrix Conduit](https://conduit.rs/)
- [meilisearch](https://www.meilisearch.com/)
- [Prometheus](https://prometheus.io/)
- [Prometheus Blackbox Exporter](https://github.com/prometheus/blackbox_exporter)
- [Traefik whoami](https://traefik.io/)

These services are supposed to feel like services backed by native Guix packages, please report any inconsistency you may find.

Here's how you would use some of the services from this channel in your `operating-system` record:

``` scheme
(use-modules (oci services prometheus)
             (oci services grafana))

(operating-system
 (services
        ;; Blackbox exporter OCI backed Guix System service
  (list (service oci-blackbox-exporter-service-type
                 (oci-blackbox-exporter-configuration
                  (network "host")
                  (file
                   (plain-file "modules.yml"
                               "
modules:
  http_2xx:
    prober: http
    http:
      preferred_ip_protocol: ip4
  http_post_2xx:
    prober: http
    http:
      method: POST
  icmp:
    prober: icmp
  icmp_ttl5:
    prober: icmp
    timeout: 5s
    icmp:
      ttl: 5\n"))))

        ;; Prometheus OCI backed Guix System service
        (service oci-prometheus-service-type
                 (oci-prometheus-configuration
                  (image "prom/prometheus:v2.45.0")
                  (network "host")
                  (record
                   (prometheus-configuration
                    (global
                     (prometheus-global-configuration
                      (scrape-interval "30s")
                      (scrape-timeout "12s")))
                    (scrape-configs
                     (list
                      (prometheus-scrape-configuration
                       (job-name "prometheus")
                       (static-configs
                        (list (prometheus-static-configuration
                               (targets '("localhost:9090"))))))
                      (prometheus-scrape-configuration
                       (job-name "blackbox")
                       (static-configs
                        (list (prometheus-static-configuration
                               (targets '("localhost:9115"))))))
                      (prometheus-scrape-configuration
                       (job-name "blackbox-icmp")
                       (metrics-path "/probe")
                       (extra-content "    params:
      module: [icmp]
    static_configs:
      - targets:
        - 1.1.1.1
    relabel_configs:
      - source_labels: [__address__]
        target_label: __param_target
      - source_labels: [__param_target]
        target_label: instance
      - target_label: __address__
        replacement: localhost:9115 # The blackbox exporter's real hostname:port."))))))))

        ;; Grafana OCI backed Guix System service
        (service oci-grafana-service-type
                 (oci-grafana-configuration
                  (image "bitnami/grafana:10.1.5")
                  (network "host"))))))
```

## Documentation

The `gocix` manual [lives in this repository](https://github.com/fishinthecalculator/gocix/blob/main/doc/README.md). It contains API documentation for all services available in the channel.

## What is a Guix channel?

A [channel](https://guix.gnu.org/en/manual/devel/en/guix.html#Channels) is roughly the Guix equivalent of Ubuntu's PPAs or container registries. It's a software repository providing Guix package and service definitions.

You can search for package and service definitions from this channel and many others at [toys.whereis.social](https://toys.whereis.social).

### Configure

To configure Guix for using this channel you need to create a `.config/guix/channels.scm` file with the following content:

``` scheme
(cons* (channel
        (name 'gocix)
        (url "https://github.com/fishinthecalculator/gocix")
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
        (url "https://github.com/fishinthecalculator/gocix")
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

If you have commit access please remember to setup the authentication hook with

```bash
guix git authenticate --cache-key=channels/gocix cdb78996334c4f63304ecce224e95bb96bfd4c7d '8D10 60B9 6BB8 292E 829B  7249 AED4 1CC1 93B7 01E2'
```

## License

Unless otherwise stated all the files in this repository are to be considered under the GPL 3.0 terms. You are more than welcome to open issues or send patches.
