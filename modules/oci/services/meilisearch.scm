;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024, 2025 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (oci services meilisearch)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services containers)
  #:use-module (gnu system shadow)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (sops utils)
  #:use-module (oci build utils)
  #:use-module (oci services configuration)
  #:use-module (srfi srfi-1)
  #:use-module (guix i18n)
  #:use-module (ice-9 match)
  #:export (oci-meilisearch-configuration
            oci-meilisearch-configuration?
            oci-meilisearch-configuration-fields
            oci-meilisearch-configuration-image
            oci-meilisearch-configuration-port
            oci-meilisearch-configuration-shepherd-requirement
            oci-meilisearch-configuration-datadir
            oci-meilisearch-configuration-database-path
            oci-meilisearch-configuration-network
            oci-meilisearch-configuration-extra-variables
            oci-meilisearch-configuration->oci-container-configuration
            oci-meilisearch-service-type))

;; Some of this code comes from the Guix manual.
;; Check it out! It's pretty cool.

(define meilisearch-tag
  "v1.14")

(define meilisearch-image
  (string-append "docker.io/getmeili/meilisearch:" meilisearch-tag))

(define (serialize-string field-name value)
  (serialize-string-environment-variable field-name value #:prefix "MEILISEARCH_"))

(define serialize-maybe-string serialize-string)

(define (serialize-boolean field-name value)
  (serialize-boolean-environment-variable field-name value #:prefix "MEILISEARCH_"))

(define-maybe string)

(define-configuration/no-serialization oci-meilisearch-configuration
  (image
   (string meilisearch-image)
   "The image to use for the OCI backed Shepherd service.")
  (port
   (string "7700")
   "The port where meilisearch will be exposed.")
  (shepherd-requirement
   (list '(user-processes))
   "A list of Shepherd services that will be waited for before starting
Meilisearch.")
  (master-key
   (string)
   "The file name of the secrets containing the instance's master key,
automatically protecting all routes except GET /health.  This means you will
need a valid API key to access all other endpoints.")
  (datadir
   (string "/var/lib/meilisearch/meili_data")
   "The directory where meilisearch writes state.")
  (database-path
   (string "/var/lib/meilisearch/data.ms")
   "The directory used by meilisearch database to store state.")
  (network
   (maybe-string)
   "The docker network where the meilisearch container will be attached. When
equal to \"host\" the @code{port} field will not be mapped into the container's
one.")
  (extra-variables
   (list '())
   "A list of pairs representing any extra environment variable that should be
set inside the container.  Refer to the
@uref{upstream,https://www.meilisearch.com/docs/learn/configuration/instance_options}
documentation for more details."))

(define (%meilisearch-secrets config)
  (list (oci-meilisearch-configuration-master-key config)))

(define %meilisearch-secrets-variables
  '("MEILI_MASTER_KEY"))

(define (%meilisearch-secrets-specs config)
  (zip %meilisearch-secrets-variables (%meilisearch-secrets config)))

(define (%meilisearch-activation config)
  "Return an activation gexp for Meilisearch."
  (let* ((datadir (oci-meilisearch-configuration-datadir config))
         (database-path
          (oci-meilisearch-configuration-database-path config)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((database-path #$database-path)
               (datadir #$datadir)
               (user (getpwnam "oci-container"))
               (uid (passwd:uid user))
               (gid (passwd:gid user)))
          ;; Setup datadirs
          (map (lambda (dir)
                 (mkdir-p dir)
                 (chown dir uid gid))
               (list datadir database-path))))))

(define oci-meilisearch-configuration->oci-container-configuration
  (lambda (config)
    (let* ((datadir
            (oci-meilisearch-configuration-datadir config))
           (database-path
            (oci-meilisearch-configuration-database-path config))
           (extra-variables
            (oci-meilisearch-configuration-extra-variables config))
           (network
            (oci-meilisearch-configuration-network config))
           (image
            (oci-meilisearch-configuration-image config))
           (port
            (oci-meilisearch-configuration-port config))
           (requirement
            (oci-meilisearch-configuration-shepherd-requirement config))
           (secrets
            (secrets-volume-mappings
             (%meilisearch-secrets config)))
           (container-config
            (oci-container-configuration
             (image image)
             (requirement requirement)
             (entrypoint "/sbin/tini")
             (command
              `("--" "sh" "-c"
                ,(sops-secrets-sh-command-wrapper
                  (%meilisearch-secrets-specs config)
                  '("/bin/meilisearch"))))
             (environment
              (append
               '(("MEILI_NO_ANALYTICS" . "true"))
               extra-variables))
             (ports
              `((,port . ,port)))
             (volumes
              `((,datadir . "/meili_data")
                (,database-path . "/data.ms")
                ,@secrets)))))
      (list
       (if (maybe-value-set? network)
           (oci-container-configuration
            (inherit container-config)
            (ports '())
            (network network))
           container-config)))))

(define oci-meilisearch-service-type
  (service-type (name 'meilisearch)
                (extensions (list (service-extension oci-service-type
                                                     (lambda (config)
                                                       (oci-extension
                                                        (containers
                                                         (oci-meilisearch-configuration->oci-container-configuration config)))))
                                  (service-extension activation-service-type
                                                     %meilisearch-activation)))
                (description
                 "This service install a OCI backed Meilisearch Shepherd Service.")))
