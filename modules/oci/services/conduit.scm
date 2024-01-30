;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (oci services conduit)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu system shadow)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (ice-9 match)
  #:use-module (oci services configuration)
  #:use-module (oci services docker)
  #:export (conduit-configuration
            conduit-configuration?
            conduit-configuration-fields
            conduit-configuration-image
            conduit-configuration-port
            conduit-configuration-server-name
            conduit-configuration-database-path
            conduit-configuration-database-backend
            conduit-configuration-max-request-size
            conduit-configuration-allow-registration?
            conduit-configuration-allow-federation?
            conduit-configuration-allow-check-for-updates?
            conduit-configuration-trusted-servers
            conduit-configuration-max-concurrent-requests
            conduit-configuration-address
            conduit-configuration-log

            conduit-configuration->oci-container-environment
            conduit-configuration->oci-container-configuration

            oci-conduit-service-type))

;; Some of this code comes from the Guix manual.
;; Check it out! It's pretty cool.

(define conduit-tag
  "v0.6.0")

(define conduit-image
  (string-append "matrixconduit/matrix-conduit:" conduit-tag))

(define (serialize-string field-name value)
  (serialize-environment-variable field-name value #:prefix "CONDUIT_"))

(define serialize-maybe-string serialize-string)

(define (serialize-boolean field-name value)
  (serialize-boolean-environment-variable field-name value #:prefix "CONDUIT_"))

(define (serialize-list-of-strings field-name value)
  (serialize-string field-name (format-json-list value)))

(define-maybe/no-serialization string)

(define-configuration/no-serialization conduit-configuration
  (image
   (string conduit-image)
   "The image to use for the OCI backed Shepherd service.")
  (port
   (string "6167")
   "The port where conduit will be exposed.")
  (server-name
   (maybe-string)
   "The fully qualified domain name where conduit will be exposed.")
  (database-path
   (string "/var/lib/matrix-conduit")
   "The directory where conduit writes state.")
  (database-backend
   (string "rocksdb")
   "The database backend used by conduit.")
  (max-request-size
   (string "20000000")
   "Expressed in bytes. The default is ~20MB.")
  (allow-registration?
   (boolean #f)
   "Whether to allow new users to sign up to the conduit instance.")
  (allow-federation?
   (boolean #t)
   "Whether to federate the conduit instance with others in the Matrix network.")
  (allow-check-for-updates?
   (boolean #f)
   "Whether conduit will look for new updates.")
  (trusted-servers
   (list-of-strings '("matrix.org"))
   "The list of trusted Matrix servers.")
  (max-concurrent-requests
   (string "100")
   "The maximum number of concurrent requests handled concurrently by conduit.")
  (address
   (string "0.0.0.0")
   "The ip address where conduit will bind for connections.")
  (log
   (string "warn,rocket=off,_=off,sled=off")
   "The logging configuration for conduit."))

(define %conduit-accounts
  (list (user-group
         (system? #t)
         (name "conduit")
         (id 1000))
        (user-account
         (name "conduit")
         (comment "Conduit's Service Account")
         (group "conduit")
         (supplementary-groups '("tty"))
         (system? #t)
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (%conduit-activation config)
  "Return an activation gexp for Conduit."
  (let* ((datadir (conduit-configuration-database-path config)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((user (getpwnam "conduit"))
               (uid (passwd:uid user))
               (gid (passwd:gid user))
               (datadir #$datadir))
          ;; Setup datadir
          (mkdir-p datadir)
          (chown datadir uid gid)
          (chmod datadir #o770)))))

(define conduit-configuration->oci-container-environment
  (lambda (config)
    (filter (compose not null?)
            (map (lambda (f)
                   (let ((field-name (configuration-field-name f))
                         (type (configuration-field-type f))
                         (value ((configuration-field-getter f) config)))
                     (if (not (eq? field-name 'image))
                         (match type
                           ('string
                            (serialize-string field-name value))
                           ('maybe-string
                            (serialize-maybe-string field-name value))
                           ('list-of-strings
                            (serialize-list-of-strings field-name value))
                           ('boolean
                            (serialize-boolean field-name value))
                           (_
                            (raise
                             (formatted-message
                              (G_ "Unknown conduit-configuration field type: ~a")
                              type))))
                         '())))
                 conduit-configuration-fields))))

(define conduit-configuration->oci-container-configuration
  (lambda (config)
    (let ((datadir (conduit-configuration-database-path config))
          (environment
           (conduit-configuration->oci-container-environment config))
          (image
           (conduit-configuration-image config))
          (port
           (conduit-configuration-port config)))
      (list (oci-container-configuration
             (image image)
             (environment environment)
             (ports
              `((,port . ,port)))
             (volumes
              `((,datadir . ,datadir))))))))

(define oci-conduit-service-type
  (service-type (name 'conduit)
                (extensions (list (service-extension oci-container-service-type
                                                     conduit-configuration->oci-container-configuration)
                                  (service-extension account-service-type
                                                     (const %conduit-accounts))
                                  (service-extension activation-service-type
                                                     %conduit-activation)))
                (default-value (conduit-configuration))
                (description
                 "This service install a OCI backed Conduit Shepherd Service.")))
