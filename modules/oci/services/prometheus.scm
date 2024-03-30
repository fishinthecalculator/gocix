;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023, 2024 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (oci services prometheus)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix records)
  #:use-module (oci services configuration)
  #:use-module (oci services docker)
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
            prometheus-configuration-extra-content

            oci-prometheus-service-type
            oci-prometheus-configuration
            oci-prometheus-configuration?
            oci-prometheus-configuration-fields
            oci-prometheus-configuration-datadir
            oci-prometheus-configuration-network
            oci-prometheus-configuration-file
            oci-prometheus-configuration-record
            oci-prometheus-configuration-image
            oci-prometheus-configuration-port
            oci-prometheus-configuration->oci-container-configuration
            %prometheus-accounts
            %prometheus-activation

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
  "v2.45.0")

(define prometheus-image
  (string-append "prom/prometheus:" prometheus-tag))

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
          static-configs))
     "\n"))

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
  #~(string-append "scrape_configs:\n"
                   #$@(map serialize-prometheus-scrape-configuration
                           value)
                   "\n"))

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
  (datadir
   (string "/var/lib/prometheus")
   "The directory where prometheus writes state.")
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
  (port
   (string "9000")
   "This host port will be mapped onto the Prometheus dashboard configured port
inside the container.")
  (metrics-port
   (string "9090")
   "This host port will be mapped onto the Prometheus health endpoint configured
port inside the container.")
  (no-serialization))

(define %prometheus-accounts
  (list (user-group
         (name "prometheus")
         (id 65534)
         (system? #t))
        (user-account
          (name "prometheus")
          (comment "Prometheus's Service Account")
          (uid 65534)
          (group "prometheus")
          (supplementary-groups '("tty"))
          (system? #t)
          (home-directory "/var/empty")
          (shell (file-append shadow "/sbin/nologin")))))

(define (%prometheus-activation config)
  "Return an activation gexp for Prometheus."
  (let ((datadir (oci-prometheus-configuration-datadir config)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((user (getpwnam "prometheus"))
               (uid (passwd:uid user))
               (gid (passwd:gid user))
               (datadir #$datadir))
          (mkdir-p datadir)
          (chown datadir uid gid)))))

(define oci-prometheus-configuration->oci-container-configuration
  (lambda (config)
    (let* ((datadir
            (oci-prometheus-configuration-datadir config))
           (network
            (oci-prometheus-configuration-network config))
           (image
            (oci-prometheus-configuration-image config))
           (port
            (oci-prometheus-configuration-port config))
           (metrics-port
            (oci-prometheus-configuration-metrics-port config))
           (prometheus.yml
            (let ((file (oci-prometheus-configuration-file config))
                  (record (oci-prometheus-configuration-record config)))
              (if (maybe-value-set? file)
                  file
                  (if (maybe-value-set? record)
                   (pt-serialize-prometheus-configuration 'record record)
                   (raise
                    (G_ "oci-prometheus-configuration: You must set either the file or the record field but both are unset!"))))))
           (container-config
            (oci-container-configuration
             (command
              '("--web.enable-lifecycle"
                "--config.file=/etc/prometheus/prometheus.yml"
                "--web.enable-admin-api"))
             (image image)
             (ports
              `((,port . "9000")
                (,metrics-port . "9090")))
             (volumes
              `((,datadir . "/prometheus")
                (,prometheus.yml . "/etc/prometheus/prometheus.yml:ro"))))))

      (list
       (if (maybe-value-set? network)
           (oci-container-configuration
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
                (extensions (list (service-extension oci-container-service-type
                                                     oci-prometheus-configuration->oci-container-configuration)
                                  (service-extension account-service-type
                                                     (const %prometheus-accounts))
                                  (service-extension activation-service-type
                                                     %prometheus-activation)))
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
   "The docker network where the grafana container will be attached. When equal
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
            (oci-container-configuration
             (image image)
             (ports
              `((,port . "9115")))
             (volumes
              `((,datadir . "/blackbox-exporter")
                (,blackbox-exporter.yml . "/opt/bitnami/blackbox-exporter/conf/config.yml:ro"))))))

      (list
       (if (maybe-value-set? network)
           (oci-container-configuration
            (inherit container-config)
            (ports '())
            (network network))
           container-config)))))

(define oci-blackbox-exporter-service-type
  (service-type (name 'blackbox-exporter)
                (extensions (list (service-extension oci-container-service-type
                                                     oci-blackbox-exporter-configuration->oci-container-configuration)
                                  (service-extension activation-service-type
                                                     %blackbox-exporter-activation)))
                (description
                 "This service install a OCI backed Blackbox Exporter Shepherd Service.")))
