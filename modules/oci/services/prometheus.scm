;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (oci services prometheus)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services docker)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:export (oci-prometheus-service-type
            oci-prometheus-configuration
            oci-prometheus-configuration?
            oci-prometheus-configuration-fields
            oci-prometheus-configuration-datadir
            oci-prometheus-configuration-file
            oci-prometheus-configuration-image
            oci-prometheus-configuration-port
            oci-prometheus-configuration->oci-container-configuration
            %prometheus-accounts
            %prometheus-activation))

(define prometheus-tag
  "v2.45.0")

(define prometheus-image
  (string-append "prom/prometheus:" prometheus-tag))

(define prometheus-file
  (plain-file "prometheus.yml"
              "global:
  scrape_interval: 30s
  scrape_timeout: 12s

scrape_configs:
  - job_name: prometheus
    metrics_path: /metrics
    static_configs:
      - targets: ['localhost:9090','localhost:9100']\n"))

(define-configuration oci-prometheus-configuration
  (datadir
   (string "/var/lib/prometheus")
   "The directory where prometheus writes state.")
  (file
   (file-like prometheus-file)
   "The configuration file to use for the OCI backed Shepherd service.")
  (image
   (string prometheus-image)
   "The image to use for the OCI backed Shepherd service.")
  (port
   (string "9000")
   "The port where prometheus will be exposed.")
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
    (let ((datadir
           (oci-prometheus-configuration-datadir config))
          (image
           (oci-prometheus-configuration-image config))
          (port
           (oci-prometheus-configuration-port config))
          (prometheus.yml
           (oci-prometheus-configuration-file config)))
      (list (oci-container-configuration
             (command
              '("--web.enable-lifecycle"
                "--config.file=/etc/prometheus/prometheus.yml"
                "--web.enable-admin-api"))
             (image image)
             (network "host")
             (ports
              `((,port . "9000")
                ("9090" . "9090")))
             (volumes
              `((,datadir . "/prometheus")
                (,prometheus.yml . "/etc/prometheus/prometheus.yml:ro"))))))))

(define oci-prometheus-service-type
  (service-type (name 'oci-prometheus)
                (extensions (list (service-extension oci-container-service-type
                                                     oci-prometheus-configuration->oci-container-configuration)
                                  (service-extension account-service-type
                                                     (const %prometheus-accounts))
                                  (service-extension activation-service-type
                                                     %prometheus-activation)))
                (default-value (oci-prometheus-configuration))
                (description
                 "This service install a OCI backed Prometheus Shepherd Service.")))
