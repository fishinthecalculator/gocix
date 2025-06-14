;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023-2025 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (oci services prometheus)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module ((gnu services docker) #:prefix mainline:)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix records)
  #:use-module (oci services configuration)
  #:use-module (oci services containers)
  #:use-module (srfi srfi-1)
  #:export (%prometheus-file

            prometheus-extension
            prometheus-extension?
            prometheus-extension-scrape-jobs

            prometheus-static-configuration
            prometheus-static-configuration?
            prometheus-static-configuration-fields
            prometheus-static-configuration-targets
            prometheus-static-configuration-extra-content

            prometheus-scrape-configuration
            prometheus-scrape-configuration?
            prometheus-scrape-configuration-fields
            prometheus-scrape-configuration-job-name
            prometheus-scrape-configuration-metrics-path
            prometheus-scrape-configuration-static-configs
            prometheus-scrape-configuration-extra-content

            prometheus-global-configuration
            prometheus-global-configuration?
            prometheus-global-configuration-fields
            prometheus-global-configuration-scrape-timeout
            prometheus-global-configuration-scrape-interval
            prometheus-global-configuration-extra-content

            prometheus-configuration
            prometheus-configuration?
            prometheus-configuration-fields
            prometheus-configuration-global
            prometheus-configuration-scrape-configs
            prometheus-configuration-retention-time
            prometheus-configuration-retention-size
            prometheus-configuration-extra-content

            oci-prometheus-service-type
            oci-prometheus-configuration
            oci-prometheus-configuration?
            oci-prometheus-configuration-fields
            oci-prometheus-configuration-runtime
            oci-prometheus-configuration-datadir
            oci-prometheus-configuration-network
            oci-prometheus-configuration-file
            oci-prometheus-configuration-record
            oci-prometheus-configuration-image
            oci-prometheus-configuration-log-file
            oci-prometheus-configuration-port
            oci-prometheus-configuration->oci-container-configuration
            prometheus-accounts
            prometheus-activation

            oci-blackbox-exporter-configuration
            oci-blackbox-exporter-configuration?
            oci-blackbox-exporter-configuration-fields
            oci-blackbox-exporter-configuration-image
            oci-blackbox-exporter-configuration-datadir
            oci-blackbox-exporter-configuration-file
            oci-blackbox-exporter-configuration-network
            oci-blackbox-exporter-configuration-port

            %blackbox-exporter-activation
            oci-blackbox-exporter-configuration->oci-container-configuration
            oci-blackbox-exporter-service-type))

(define prometheus-tag
  "v3.2.1")

(define prometheus-image
  (string-append "docker.io/prom/prometheus:" prometheus-tag))

(define %prometheus-file
  (plain-file "prometheus.yml"
              "global:
  scrape_interval: 30s
  scrape_timeout: 12s

scrape_configs:
  - job_name: prometheus
    metrics_path: /metrics
    static_configs:
      - targets: ['localhost:9090','localhost:9100']\n"))

(define (string-or-volume? value)
  (or (string? value)
      (oci-volume-configuration? value)))
(define-maybe/no-serialization string-or-volume)

(define (serialize-string field-name value)
  (serialize-yaml-string field-name value #:indentation "  "))

(define-configuration/no-serialization prometheus-static-configuration
  (targets
   (list-of-strings '())
   "The target hosts that will be scraped for metrics.")
  (extra-content
   (string "")
   "Everything you want to manually append to this @code{static_config} field."))

(define (serialize-prometheus-static-configuration field-name static-configs)
  (if (> (length static-configs) 0)
      #~(string-append
         "    static_configs:\n"
         (string-append
          #$@(map
              (lambda (value)
                #~(string-append
                   #$(configuration->yaml-block value prometheus-static-configuration-fields
                                                #:indentation "      "
                                                #:excluded '(extra-content))
                   #$(prometheus-static-configuration-extra-content value)))
              static-configs)))
      ""))

(define list-of-prometheus-static-configurations?
  (list-of prometheus-static-configuration?))

(define-configuration/no-serialization prometheus-scrape-configuration
  (job-name
   (string)
   "The name of the scrape job.")
  (metrics-path
   (string "/metrics")
   "The path where this job will scrape metrics.")
  (static-configs
   (list-of-prometheus-static-configurations '())
   "The list of static configurations for this job.")
  (extra-content
   (string "")
   "Everything you want to manually append to this @code{scrape_config} field."))

(define (serialize-prometheus-scrape-configuration value)
  #~(string-append
     #$(configuration->yaml-block value prometheus-scrape-configuration-fields
                                  #:indentation "  "
                                  #:excluded '(extra-content static-configs))
     #$(serialize-prometheus-static-configuration
        'static-configs
        (prometheus-scrape-configuration-static-configs value))
     #$(prometheus-scrape-configuration-extra-content value)))

(define (pt-serialize-list-of-prometheus-scrape-configurations field-name value)
  (if (> (length value) 0)
      #~(string-append "scrape_configs:\n"
                       #$@(map serialize-prometheus-scrape-configuration
                               value))
      ""))

(define list-of-prometheus-scrape-configurations?
  (list-of prometheus-scrape-configuration?))

(define-configuration/no-serialization prometheus-global-configuration
  (scrape-interval
   (string "30s")
   "Prometheus' @code{scrape_interval} field.")
  (scrape-timeout
   (string "12s")
   "Prometheus' @code{scrape_timeout} field.")
  (extra-content
   (string "")
   "Everything you want to manually append to the @code{global} section."))

(define (pt-serialize-prometheus-global-configuration field-name value)
  #~(string-append
     "global:\n"
     #$(configuration->yaml-block value prometheus-global-configuration-fields
                                  #:sequence? #f
                                  #:indentation "  "
                                  #:excluded '(extra-content))
     #$(prometheus-global-configuration-extra-content value)
     "\n"))

(define pt-serialize-string
  serialize-yaml-string)

(define-maybe string)
(define-maybe file-like)

(define-configuration prometheus-configuration
  (global
   (prometheus-global-configuration (prometheus-global-configuration))
   "Prometheus' @code{global} section.")
  (scrape-configs
   (list-of-prometheus-scrape-configurations '())
   "Prometheus' @code{scrape_configs} section.")
  (retention-time
   (string "15d")
   "How long to retain samples in storage."
   (serializer empty-serializer))
  (retention-size
   (maybe-string)
   "The maximum number of bytes of storage blocks to retain.  The oldest data
will be removed first.  Units supported: B, KB, MB, GB, TB, PB, EB.
Based on powers-of-2, so 1KB is 1024B."
   (serializer empty-serializer))
  (extra-content
   (string "")
   "Everything you want to manually append to the configuration file."
   (serializer empty-serializer))
  (prefix pt-))

(define (pt-serialize-prometheus-configuration field-name configuration)
  (if (maybe-value-set? configuration)
      (mixed-text-file
       "prometheus.yml"
       (serialize-configuration
        configuration prometheus-configuration-fields)
       (prometheus-configuration-extra-content configuration)
       "\n")
      ""))

(define serialize-prometheus-configuration
  pt-serialize-prometheus-configuration)
(define-maybe prometheus-configuration)

(define-configuration oci-prometheus-configuration
  (runtime
   (symbol 'docker)
   "The OCI runtime to be used for this service.")
  (datadir
   (maybe-string-or-volume)
   "The directory where prometheus writes state.  It can be either an
@code{oci-volume-configuration} representing the OCI volume where Prometheus will
write state, or a string representing a file system path in the host system which
will be mapped inside the container.  By default it is @code{\"/var/lib/prometheus\"}.")
  (file
   (maybe-file-like)
   "The configuration file to use for the OCI backed Shepherd service.")
  (record
   (maybe-prometheus-configuration)
   "The configuration record to use for the OCI backed Shepherd service.  If
the @code{file} field is set, this field will be ignored.")
  (image
   (string prometheus-image)
   "The image to use for the OCI backed Shepherd service.")
  (network
   (maybe-string)
   "The docker network where the grafana container will be attached. When equal
to \"host\" the @code{port} field will be ignored.")
  (log-file
   (maybe-string)
   "When @code{log-file} is set, it names the file to which the service’s
standard output and standard error are redirected.  @code{log-file} is created
if it does not exist, otherwise it is appended to.  By default it is
@code{\"/var/log/prometheus.log\"}.")
  (port
   (string "9000")
   "This host port will be mapped onto the Prometheus dashboard configured port
inside the container.")
  (metrics-port
   (string "9090")
   "This host port will be mapped onto the Prometheus health endpoint configured
port inside the container.")
  (no-serialization))

(define (oci-prometheus-log-file config)
  (define maybe-log-file
    (oci-prometheus-configuration-log-file config))
  (if (maybe-value-set? maybe-log-file)
      maybe-log-file
      "/var/log/prometheus.log"))

(define (oci-prometheus-datadir config)
  (define maybe-datadir
    (oci-prometheus-configuration-datadir config))
  (if (maybe-value-set? maybe-datadir)
      maybe-datadir
      "/var/lib/prometheus"))

(define (prometheus-accounts config)
  (let ((runtime (oci-prometheus-configuration-runtime config)))
    (list (user-group
           (name "prometheus")
           (id 65534)
           (system? #t))
          (user-account
            (name "prometheus")
            (comment "Prometheus's Service Account")
            (uid 65534)
            (group (if (eq? 'podman runtime) "users" "prometheus"))
            (supplementary-groups '("tty"))
            (system? (eq? 'docker runtime))
            (home-directory "/var/empty")
            (shell (file-append shadow "/sbin/nologin"))))))

(define (prometheus-activation config)
  "Return an activation gexp for Prometheus."
  (let* ((datadir (oci-prometheus-datadir config))
         (runtime (oci-prometheus-configuration-runtime config)))
    #~(begin
        (use-modules (guix build utils))
        #$(if (string? datadir)
              #~(let* ((user (getpwnam
                              (if #$(eq? 'podman runtime)
                                  "oci-container" "prometheus")))
                       (uid (passwd:uid user))
                       (gid (passwd:gid user))
                       (datadir #$datadir))
                  ;; Setup datadir
                  (mkdir-p datadir)
                  (chown datadir uid gid)
                  (if #$(eq? 'podman runtime)
                      (chmod datadir #o660)
                      (chmod datadir #o755)))
              #~(begin)))))

(define oci-prometheus-configuration->oci-container-configuration
  (lambda (config)
    (let* ((datadir (oci-prometheus-datadir config))
           (log-file (oci-prometheus-log-file config))
           (network
            (oci-prometheus-configuration-network config))
           (image
            (oci-prometheus-configuration-image config))
           (port
            (oci-prometheus-configuration-port config))
           (metrics-port
            (oci-prometheus-configuration-metrics-port config))
           (file (oci-prometheus-configuration-file config))
           (record (oci-prometheus-configuration-record config))
           (retention-time
            (if (maybe-value-set? record)
                (prometheus-configuration-retention-time record)
                "15d"))
           (retention-size
            (and (maybe-value-set? record)
                 (prometheus-configuration-retention-size record)))
           (prometheus.yml
            (if (maybe-value-set? file)
                file
                (if (maybe-value-set? record)
                    (pt-serialize-prometheus-configuration 'record record)
                    (raise
                     (G_ "oci-prometheus-configuration: You must set either the file or the record field but both are unset!")))))
           (container-config
            (mainline:oci-container-configuration
             (command
              `("--web.enable-lifecycle"
                "--config.file=/etc/prometheus/prometheus.yml"
                "--web.enable-admin-api"
                ,(string-append "--storage.tsdb.retention.time="
                                retention-time)
                ,@(if (and retention-size (maybe-value-set? retention-size))
                      (string-append "--storage.tsdb.retention.size="
                                     retention-size)
                      '())))
             (image image)
             (log-file log-file)
             (ports
              `((,port . "9000")
                (,metrics-port . "9090")))
             (volumes
              `((,(if (string? datadir)
                      datadir
                      (oci-volume-configuration-name datadir))
                 . "/prometheus")
                (,prometheus.yml . "/etc/prometheus/prometheus.yml:ro"))))))

      (list
       (if (maybe-value-set? network)
           (mainline:oci-container-configuration
            (inherit container-config)
            (ports '())
            (network network))
           container-config)))))

(define-record-type* <prometheus-extension>
  prometheus-extension make-prometheus-extension
  prometheus-extension?
  (scrape-configs           prometheus-extension-scrape-configs
                            (default '())))         ; list of prometheus-scrape-configurations

(define (prometheus-extension-merge a b)
  (prometheus-extension
   (scrape-configs (append (prometheus-extension-scrape-configs a)
                           (prometheus-extension-scrape-configs b)))))

(define oci-prometheus-service-type
  (service-type (name 'prometheus)
                (extensions (list (service-extension oci-service-type
                                                     (lambda (config)
                                                       (oci-extension
                                                        (volumes
                                                         (let ((datadir (oci-prometheus-configuration-datadir config)))
                                                           (if (oci-volume-configuration? datadir) (list datadir) '())))
                                                        (containers
                                                         (oci-prometheus-configuration->oci-container-configuration config)))))
                                  (service-extension account-service-type
                                                     prometheus-accounts)
                                  (service-extension activation-service-type
                                                     prometheus-activation)))
                (compose
                 (lambda (args) (fold prometheus-extension-merge (prometheus-extension) args)))
                (extend
                 (lambda (config extension)
                   (let ((file (oci-prometheus-configuration-file config))
                         (configuration-record
                          (oci-prometheus-configuration-record config)))
                     (if (maybe-value-set? file)
                         (raise
                          (G_ "You can't extend the oci-prometheus-service-type if the file field of oci-prometheus-configuration is set!"))
                         (if (maybe-value-set? configuration-record)
                             (oci-prometheus-configuration
                              (inherit config)
                              (record
                               (prometheus-configuration
                                (inherit configuration-record)
                                (scrape-configs
                                 (append (prometheus-extension-scrape-configs extension)
                                         (prometheus-configuration-scrape-configs configuration-record))))))
                             (raise
                              (G_ "The record field of oci-prometheus-configuration must set to be able to extend it!")))))))
                (description
                 "This service install a OCI backed Prometheus Shepherd Service.")))

;; Blackbox exporter

(define blackbox-exporter-tag
  "0.24.0")

(define blackbox-exporter-image
  (string-append "bitnami/blackbox-exporter:" blackbox-exporter-tag))

(define-configuration oci-blackbox-exporter-configuration
  (datadir
   (string "/var/lib/blackbox-exporter")
   "The directory where blackbox-exporter writes state.")
  (file
   (file-like)
   "The configuration file to use for Blackbox Exporter.")
  (image
   (string blackbox-exporter-image)
   "The image to use for the OCI backed Shepherd service.")
  (network
   (maybe-string)
   "The docker network where the container will be attached. When equal
to \"host\" the @code{port} field will be ignored.")
  (port
   (string "9115")
   "This host port will be mapped onto the HTTP port
inside the container.  If @code{network} is set this field will be ignored.")
  (no-serialization))

(define (%blackbox-exporter-activation config)
  "Return an activation gexp for Blackbox Exporter."
  (let ((datadir (oci-blackbox-exporter-configuration-datadir config)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((datadir #$datadir))
          (mkdir-p datadir)
          (chmod datadir #o764)))))

(define oci-blackbox-exporter-configuration->oci-container-configuration
  (lambda (config)
    (let* ((datadir
            (oci-blackbox-exporter-configuration-datadir config))
           (network
            (oci-blackbox-exporter-configuration-network config))
           (image
            (oci-blackbox-exporter-configuration-image config))
           (port
            (oci-blackbox-exporter-configuration-port config))
           (blackbox-exporter.yml
            (oci-blackbox-exporter-configuration-file config))
           (container-config
            (mainline:oci-container-configuration
             (image image)
             (ports
              `((,port . "9115")))
             (volumes
              `((,(if (string? datadir)
                      datadir
                      (oci-volume-configuration-name datadir))
                 . "/blackbox-exporter")
                (,blackbox-exporter.yml . "/opt/bitnami/blackbox-exporter/conf/config.yml:ro"))))))

      (list
       (if (maybe-value-set? network)
           (mainline:oci-container-configuration
            (inherit container-config)
            (ports '())
            (network network))
           container-config)))))

(define oci-blackbox-exporter-service-type
  (service-type (name 'blackbox-exporter)
                (extensions (list (service-extension oci-service-type
                                                     (lambda (config)
                                                      (oci-extension
                                                       (containers
                                                        (list
                                                         (oci-blackbox-exporter-configuration->oci-container-configuration
                                                          config))))))
                                  (service-extension activation-service-type
                                                     %blackbox-exporter-activation)))
                (description
                 "This service install a OCI backed Blackbox Exporter Shepherd Service.")))
