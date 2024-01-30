;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (oci services bonfire)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu system shadow)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (ice-9 match)
  #:use-module (sops secrets)
  #:use-module (sops services sops)
  #:use-module (oci services configuration)
  #:use-module (oci services docker)
  #:export (bonfire-configuration
            bonfire-configuration?
            bonfire-configuration-fields
            bonfire-configuration-flavour
            bonfire-configuration-image
            bonfire-configuration-public-port

            oci-bonfire-configuration
            oci-bonfire-configuration?
            oci-bonfire-configuration-fields
            oci-bonfire-configuration-image
            oci-bonfire-configuration-upload-data-directory
            oci-bonfire-configuration-configuration
            oci-bonfire-configuration-requirement
            oci-bonfire-configuration-secret-key-base
            oci-bonfire-configuration-signing-salt
            oci-bonfire-configuration-encryption-salt
            oci-bonfire-configuration-postgres-password
            oci-bonfire-configuration-network
            oci-bonfire-configuration-extra-variables

            bonfire-configuration->oci-container-environment
            oci-bonfire-configuration->oci-container-configuration

            oci-bonfire-service-type))

(define (bonfire-tag flavour system)
  (string-append "0.9.8-beta.26-" flavour "-" system))

(define (bonfire-image flavour system)
  (string-append "bonfirenetworks/bonfire:" (bonfire-tag flavour system)))

(define (serialize-string field-name value)
  (serialize-environment-variable field-name value))

(define serialize-maybe-string serialize-string)

(define (serialize-boolean field-name value)
  (serialize-boolean-environment-variable field-name value))

(define-maybe string)

(define-configuration/no-serialization bonfire-configuration
  (flavour
   (string "classic")
   "The flavour of the Bonfire instance.  You can refer to
@uref{mainline's images, https://bonfirenetworks.org/docs} for details.")
  (hostname
   (string)
   "The domain name where Bonfire will be exposed.")
  (public-port
   (string "4000")
   "The port where bonfire will be exposed."))

(define bonfire-configuration->oci-container-environment
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
                              (G_ "Unknown bonfire-configuration field type: ~a")
                              type))))
                         '())))
                 bonfire-configuration-fields))))

(define-configuration/no-serialization oci-bonfire-configuration
  (image
   (string (bonfire-image "classic" "amd64"))
   "The image to use for the OCI backed Shepherd service.")
  (upload-data-directory
   (string "/var/lib/bonfire/uploads")
   "Upload data directory.")
  (configuration
   (bonfire-configuration)
   "A bonfire-configuration record used to configure the Bonfire instance.")
  (requirement
   (list '(postgresql))
   "A list of Shepherd services that will be waited for before starting Bonfire.")
  (secret-key-base
   (sops-secret)
   "SECRET_KEY_BASE Bonfire secret.")
  (signing-salt
   (sops-secret)
   "SIGNING_SALT Bonfire secret.")
  (encryption-salt
   (sops-secret)
   "ENCRYPTION_SALT Bonfire secret.")
  (postgres-password
   (sops-secret)
   "POSTGRES_PASSWORD Bonfire secret.")
  (network
   (maybe-string)
   "The docker network where the bonfire container will be attached. When equal
to \"host\" the @code{port} field will not be mapped into the container's one.")
  (extra-variables
   (list '())
   "A list of pairs representing any extra environment variable that should be set inside the container. Refer to the @uref{mainline, https://bonfirenetworks.org/docs/deploy/} documentation for more details."))

(define (%bonfire-activation config)
  "Return an activation gexp for Bonfire."
  (when config
    (let* ((upload-data-directory
            (oci-bonfire-configuration-upload-data-directory config)))
      #~(begin
          (use-modules (guix build utils))
          (let* ((upload-data-directory #$upload-data-directory))
            ;; Setup datadirs
            (mkdir-p upload-data-directory))))))

(define oci-bonfire-configuration->oci-container-configuration
  (lambda (config)
    (when config
      (let* ((bonfire-config
              (oci-bonfire-configuration-configuration config))
             (upload-data-directory
              (oci-bonfire-configuration-upload-data-directory config))
             (environment
              (bonfire-configuration->oci-container-environment
               bonfire-config))
             (extra-variables
              (oci-bonfire-configuration-extra-variables config))
             (network
              (oci-bonfire-configuration-network config))
             (port
              (bonfire-configuration-public-port bonfire-config))
             (image
              (oci-bonfire-configuration-image config))
             (requirement
              (oci-bonfire-configuration-requirement config))
             (secrets
              (map (lambda (s)
                     (string-append "/run/secrets/" s))
                   '("meilisearch/master"
                     "postgres/bonfire"
                     "bonfire/secret_key_base"
                     "bonfire/signing_salt"
                     "bonfire/encryption_salt")))
             (container-config
              (oci-container-configuration
               (image image)
               (requirement `(,@requirement sops-secrets))
               (entrypoint "/bin/sh")
               (command
                `("-c" "export MEILI_MASTER_KEY=$(cat /run/secrets/meilisearch/master)
SECRET_KEY_BASE=$(cat /run/secrets/bonfire/secret_key_base)
SIGNING_SALT=$(cat /run/secrets/bonfire/signing_salt)
ENCRYPTION_SALT=$(cat /run/secrets/bonfire/encryption_salt)
POSTGRES_PASSWORD=$(cat /run/secrets/postgres/bonfire)
&& exec ./bin/bonfire start"))
               (environment
                (append
                 environment
                 extra-variables))
               (ports
                `((,port . ,port)))
               (volumes
                `((,upload-data-directory . "/opt/app/data/uploads")
                  ,@(map (lambda (s)
                           (cons s (string-append s ":ro")))
                         secrets))))))
        (list
         (if (maybe-value-set? network)
             (oci-container-configuration
              (inherit container-config)
              (network network))
             container-config))))))

(define oci-bonfire-service-type
  (service-type (name 'bonfire)
                (extensions (list (service-extension oci-container-service-type
                                                     oci-bonfire-configuration->oci-container-configuration)
                                  (service-extension sops-secrets-service-type
                                                     (lambda (config)
                                                       (list
                                                        (oci-bonfire-configuration-secret-key-base config)
                                                        (oci-bonfire-configuration-signing-salt config)
                                                        (oci-bonfire-configuration-encryption-salt config)
                                                        (oci-bonfire-configuration-postgres-password config))))
                                  (service-extension activation-service-type
                                                     %bonfire-activation)))
                (default-value #f)
                (description
                 "This service install a OCI backed Bonfire Shepherd Service.")))