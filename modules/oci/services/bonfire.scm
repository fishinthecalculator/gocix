;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (oci services bonfire)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd) ;for shepherd-action
  #:use-module (gnu system shadow)
  #:use-module (guix build-system copy)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (sops secrets)
  #:use-module (sops services sops)
  #:use-module (oci self)
  #:use-module (oci services configuration)
  #:use-module (oci services docker)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (bonfire-configuration
            bonfire-configuration?
            bonfire-configuration-fields
            bonfire-configuration-flavour
            bonfire-configuration-image
            bonfire-configuration-port
            bonfire-configuration-public-port
            bonfire-configuration-postgres-host
            bonfire-configuration-postgres-user
            bonfire-configuration-postgres-db
            bonfire-configuration-mail-from
            bonfire-configuration-mail-domain
            bonfire-configuration-mail-server
            bonfire-configuration-mail-user
            bonfire-configuration-mail-port
            bonfire-configuration-mail-ssl?

            oci-bonfire-configuration
            oci-bonfire-configuration?
            oci-bonfire-configuration-fields
            oci-bonfire-configuration-image
            oci-bonfire-configuration-upload-data-directory
            oci-bonfire-configuration-configuration
            oci-bonfire-configuration-secrets-directory
            oci-bonfire-configuration-requirement
            oci-bonfire-configuration-auto-start?
            oci-bonfire-configuration-secret-key-base
            oci-bonfire-configuration-signing-salt
            oci-bonfire-configuration-encryption-salt
            oci-bonfire-configuration-mail-password
            oci-bonfire-configuration-postgres-password
            oci-bonfire-configuration-network
            oci-bonfire-configuration-extra-variables

            bonfire-configuration->oci-container-environment
            oci-bonfire-configuration->oci-container-configuration

            oci-bonfire-service-type))

(define (bonfire-tag flavour system)
  (string-append "0.9.10-beta.62-" flavour "-" system))

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
  (postgres-host
   (string "localhost")
   "The hostname where postgres will be looked for.")
  (postgres-db
   (string "bonfire_db")
   "The database name of the Bonfire's Postgres database.")
  (postgres-user
   (string "bonfire")
   "The user name that Bonfire will use to authenticate against the Postgres database.")
  (mail-server
   (maybe-string)
   "SMTP domain of the mail server.")
  (mail-domain
   (maybe-string)
   "The bit after @ in your email.")
  (mail-user
   (maybe-string)
   "The bit before @ in your email.")
  (mail-from
   (maybe-string)
   "The email address from which Bonfire will send emails.")
  (mail-port
   (string "465")
   "The port of the SMTP service on your mail server.")
  (mail-ssl?
   (boolean #t)
   "Whether to use SSL for the connection to the SMTP server.")
  (port
   (string "4000")
   "The internal port where Bonfire will be exposed.")
  (public-port
   (string "443")
   "The public port where Bonfire will be exposed."))

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
  (log-file
   (maybe-string)
   "When @code{log-file} is set, it names the file to which the service’s
standard output and standard error are redirected.  @code{log-file} is created
if it does not exist, otherwise it is appended to.")
  (auto-start?
   (boolean #t)
   "Whether Bonfire should be started automatically by the Shepherd.  If it
is @code{#f} Bonfire has to be started manually with @command{herd start}.")
  (secrets-directory
   (string "/run/secrets")
   "The directory where secrets are looked for.")
  (secret-key-base
   (sops-secret)
   "SECRET_KEY_BASE Bonfire secret.")
  (signing-salt
   (sops-secret)
   "SIGNING_SALT Bonfire secret.")
  (encryption-salt
   (sops-secret)
   "ENCRYPTION_SALT Bonfire secret.")
  (mail-password
   (sops-secret)
   "MAIL_KEY Bonfire secret.")
  (postgres-password
   (sops-secret)
   "POSTGRES_PASSWORD Bonfire secret.")
  (meili-master-key
   (sops-secret)
   "MEILI_MASTER_KEY Bonfire secret.")
  (network
   (maybe-string)
   "The docker network where the bonfire container will be attached. When equal
to \"host\" the @code{port} field will not be mapped into the container's one.")
  (extra-variables
   (list '())
   "A list of pairs representing any extra environment variable that should be set inside the container. Refer to the @uref{mainline, https://bonfirenetworks.org/docs/deploy/} documentation for more details."))

(define (%bonfire-secrets config)
  (list (oci-bonfire-configuration-meili-master-key config)
        (oci-bonfire-configuration-postgres-password config)
        (oci-bonfire-configuration-mail-password config)
        (oci-bonfire-configuration-secret-key-base config)
        (oci-bonfire-configuration-signing-salt config)
        (oci-bonfire-configuration-encryption-salt config)))

(define %bonfire-secrets-variables
  '("MEILI_MASTER_KEY"
    "POSTGRES_PASSWORD"
    "MAIL_KEY"
    "SECRET_KEY_BASE"
    "SIGNING_SALT"
    "ENCRYPTION_SALT"))

(define (%bonfire-secrets-files config)
  (map (lambda (s)
         (string-append (oci-bonfire-configuration-secrets-directory config)
                        "/" (sops-secret->file-name s)))
       (%bonfire-secrets config)))

(define (%bonfire-secrets-specs config)
  (zip %bonfire-secrets-variables
       (%bonfire-secrets-files config)))

(define (%bonfire-activation config)
  "Return an activation gexp for Bonfire."
  (when config
    (let* ((log-file
            (oci-bonfire-configuration-log-file config))
           (upload-data-directory
            (oci-bonfire-configuration-upload-data-directory config)))
      #~(begin
          (use-modules (guix build utils))
          (let* ((upload-data-directory #$upload-data-directory))
            ;; Setup datadirs
            (mkdir-p upload-data-directory)

            (when #$(maybe-value-set? log-file)
              (let ((logs-directory (dirname #$log-file)))
                (unless (file-exists? logs-directory)
                  (mkdir-p logs-directory)))))))))

(define* (oci-bonfire-sh-command secrets-specs command)
  "Exports each one of the SECRETS-SPECS as an environment variable
and returns Bonfire's sh command."
  (string-join
   `("set -e"
     ,@(map (match-lambda
              ((variable secret)
               (string-append
                "export " variable "=\"$(cat " secret ")\"")))
            secrets-specs)
     ,(string-append "exec " command))
   "; "))

(define oci-bonfire-configuration->oci-container-configuration
  (lambda (config)
    (when config
      (let* ((auto-start?
              (oci-bonfire-configuration-auto-start? config))
             (bonfire-config
              (oci-bonfire-configuration-configuration config))
             (upload-data-directory
              (oci-bonfire-configuration-upload-data-directory config))
             (environment
              (bonfire-configuration->oci-container-environment
               bonfire-config))
             (extra-variables
              (oci-bonfire-configuration-extra-variables config))
             (log-file
              (oci-bonfire-configuration-log-file config))
             (network
              (oci-bonfire-configuration-network config))
             (port
              (bonfire-configuration-port bonfire-config))
             (image
              (oci-bonfire-configuration-image config))
             (requirement
              (oci-bonfire-configuration-requirement config))
             (secrets-directories
              (delete-duplicates
               (map (lambda (secret-file)
                      (define secret-directory (dirname secret-file))
                      (string-append secret-directory ":"
                                     secret-directory ":ro"))
                    (%bonfire-secrets-files config))))
             (container-config
              (oci-container-configuration
               (image image)
               (auto-start? auto-start?)
               (requirement `(,@requirement sops-secrets))
               (entrypoint "/bin/sh")
               (command
                `("-c" ,(oci-bonfire-sh-command (%bonfire-secrets-specs config)
                                                "./bin/bonfire start")))
               (environment
                (append
                 environment
                 '("LANG"
                   ("SEEDS_USER" . "root")
                   ("ERLANG_COOKIE" . "bonfire_cookie")
                   ("MIX_ENV" . "prod")
                   ("PLUG_BACKEND" . "bandit")
                   ("APP_NAME" . "Bonfire"))
                 extra-variables))
               (ports
                `((,port . ,port)))
               (volumes
                `((,upload-data-directory . "/opt/app/data/uploads")
                  ,@secrets-directories))
               (log-file log-file))))
        (list
         (if (maybe-value-set? network)
             (oci-container-configuration
              (inherit container-config)
              (ports '())
              (network network))
             container-config))))))

(define (bonfire-iex bonfire-config secrets-specs)
  (let* ((bash (file-append bash "/bin/bash"))
         (config
          (first
           (oci-bonfire-configuration->oci-container-configuration
            bonfire-config)))
         (image (oci-container-configuration-image config))
         (options (oci-container-configuration->options
                   config)))
    (program-file
     "bonfire-iex"
     #~(execlp #$bash #$bash "-c"
               (string-append "docker run -it --rm --name bonfire-iex "
                              (string-join (list #$@options #$image) " ")
                              " -c '"
                              #$(oci-bonfire-sh-command secrets-specs
                                                        "bin/bonfire start_iex")
                              "'")))))

(define (bonfire-utils-package config secrets-specs)
  (package
    (name "bonfire-utils")
    (version "0.0.0")
    (source (bonfire-iex config secrets-specs))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan #~'(("./bonfire-iex" "/bin/"))))
    (home-page %oci-channel-url)
    (synopsis
     "Easily interact from the CLI with gocix' Bonfire service.")
    (description
     "This package provides a simple wrapper around the @code{bonfire} OCI backed Shepherd service.
It allows for easily interacting with the Bonfire instance,
for example by starting an interactive shell attached to the Elixir process.")
    (license license:gpl3+)))

(define oci-bonfire-service-type
  (service-type (name 'bonfire)
                (extensions (list (service-extension oci-container-service-type
                                                     oci-bonfire-configuration->oci-container-configuration)
                                  (service-extension profile-service-type
                                                     (lambda (config)
                                                       (list
                                                        (bonfire-utils-package
                                                         config
                                                         (%bonfire-secrets-specs config)))))
                                  (service-extension sops-secrets-service-type
                                                     (lambda (config)
                                                       (%bonfire-secrets config)))
                                  (service-extension activation-service-type
                                                     %bonfire-activation)))
                (default-value #f)
                (description
                 "This service install a OCI backed Bonfire Shepherd Service.")))
