;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (oci services tandoor)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module ((gnu services docker) #:prefix mainline:)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 string-fun)
  #:use-module (sops secrets)
  #:use-module (sops services databases)
  #:use-module (sops services sops)
  #:use-module (oci services configuration)
  #:use-module (oci services containers)
  #:export (oci-tandoor-service-type

            tandoor-configuration
            tandoor-configuration?
            tandoor-configuration-fields
            tandoor-configuration-create-database?
            tandoor-configuration-postgres-db
            tandoor-configuration-postgres-host
            tandoor-configuration-postgres-user

            oci-tandoor-configuration
            oci-tandoor-configuration?
            oci-tandoor-configuration-fields
            oci-tandoor-configuration-runtime
            oci-tandoor-configuration-staticdir
            oci-tandoor-configuration-mediadir
            oci-tandoor-configuration-requirement
            oci-tandoor-configuration-image
            oci-tandoor-configuration-port
            oci-tandoor-configuration-secrets-directory
            oci-tandoor-configuration-postgres-password
            oci-tandoor-configuration-secret-key
            oci-tandoor-configuration-network
            oci-tandoor-configuration->oci-container-configuration

            tandoor-accounts
            tandoor-activation))

;; Some of this code comes from the Guix manual.
;; Check it out! It's pretty cool.

(define tandoor-tag
  "1.5.31-open-data-plugin")

(define tandoor-image
  (string-append "docker.io/vabene1111/recipes:" tandoor-tag))

(define-maybe string)

(define (string-or-volume? value)
  (or (string? value)
      (oci-volume-configuration? value)))
(define-maybe/no-serialization string-or-volume)

(define-configuration/no-serialization tandoor-configuration
  (create-database?
   (boolean #t)
   "Whether to create a database with the same name as the role.")
  (postgres-host
   (string "localhost")
   "The hostname where postgres will be looked for.")
  (postgres-db
   (string "tandoor_db")
   "The database name of the Tandoor's Postgres database.  When
@code{postgres-host} is equal to @code{\"localhost\"} or to a path that exists
on the filesystem, the service will assume that the database is provisioned with
Guix Systems' @code{postgresql-role-service-type}
(@pxref{Database Services,,, guix, The GNU Guix Manual}).  In this case the
@code{postgres-user} field will be ignored and this field will be used both as
database name and as an authentication user name.")
  (postgres-user
   (string "tandoor")
   "The user name that Tandoor will use to authenticate against the Postgres database."))

(define-configuration/no-serialization oci-tandoor-configuration
  (runtime
   (symbol 'docker)
   "The OCI runtime to be used for this service.")
  (staticdir
   (maybe-string-or-volume)
   "The directory where tandoor writes static files.  It can be either an
@code{oci-volume-configuration} representing the OCI volume where Tandoor will
write, or a string representing a file system path in the host system which
will be mapped inside the container.  By default it is @code{\"/opt/recipes/staticfiles\"}.")
  (mediadir
   (maybe-string-or-volume)
   "The directory where tandoor writes media files.  It can be either an
@code{oci-volume-configuration} representing the OCI volume where Tandoor will
write, or a string representing a file system path in the host system which
will be mapped inside the container.  By default it is @code{\"/opt/recipes/mediafiles\"}.")
  (configuration
   (tandoor-configuration)
   "A tandoor-configuration record used to configure the Tandoor instance.")
  (image
   (string tandoor-image)
   "The image to use for the OCI backed Shepherd service.")
  (port
   (string "8080")
   "This host port will be mapped onto the Tandoor configured port inside the container.")
  (requirement
   (list '(postgresql))
   "A list of Shepherd services that will be waited for before starting Tandoor.")
  (secrets-directory
   (string "/run/secrets")
   "The directory where secrets are looked for.")
  (postgres-password
   (sops-secret)
   "POSTGRES_PASSWORD Tandoor secret.")
  (secret-key
   (sops-secret)
   "SECRET_KEY Tandoor secret.")
  (network
   (maybe-string)
   "The docker network where the tandoor container will be attached. When equal
to \"host\" the @code{port} field will be ignored.")
  (extra-variables
   (list '())
   "A list of pairs representing any extra environment variable that should be set inside the container. Refer to the @uref{mainline, https://docs.tandoor.dev/install/docker/#docker} documentation for more details."))

(define (tandoor-configuration-local-database? config)
  (define host
    (tandoor-configuration-postgres-host config))
  (define db
    (tandoor-configuration-postgres-db config))
  (or (string=? "localhost" host) (file-exists? db)))

(define (tandoor-configuration->oci-container-environment config)
  (append
   (if (tandoor-configuration-local-database? config)
       (list (string-append "POSTGRES_USER="
                            (tandoor-configuration-postgres-db config)))
       '())
   (configuration->environment-variables config tandoor-configuration-fields
                                         #:excluded `(create-database?
                                                      ,@(if (tandoor-configuration-local-database? config)
                                                            '(postgres-user)
                                                            '())))))

(define (%tandoor-secrets config)
  (list (oci-tandoor-configuration-postgres-password config)
        (oci-tandoor-configuration-secret-key config)))

(define %tandoor-secrets-variables
  '("POSTGRES_PASSWORD"
    "SECRET_KEY"))

(define (%tandoor-secret-file config secret)
  (string-append (oci-tandoor-configuration-secrets-directory config)
                 "/" (sops-secret->file-name secret)))

(define (%tandoor-secrets-files config)
  (map (lambda (s) (%tandoor-secret-file config s))
       (%tandoor-secrets config)))

(define (%tandoor-secrets-postgres-password-file config)
  (%tandoor-secret-file
   config (oci-tandoor-configuration-postgres-password config)))

(define (%tandoor-secrets-specs config)
  (zip %tandoor-secrets-variables
       (%tandoor-secrets-files config)))

(define* (oci-tandoor-sh-command secrets-specs command)
  "Exports each one of the SECRETS-SPECS as an environment variable
and returns Tandoor's sh command."
  (string-join
   `("set -e"
     ,@(map (match-lambda
              ((variable secret)
               (string-append
                "export " variable "=\"$(cat " secret ")\"")))
            secrets-specs)
     ,(string-append "exec -a " (car command) " " command))
   "; "))

(define (tandoor-accounts config)
  (let ((runtime (oci-tandoor-configuration-runtime config)))
    (list (user-account
           (name "tandoor")
           (comment "Tandoor's Service Account")
           (group "users")
           (supplementary-groups '("tty"))
           (system? (eq? 'docker runtime))
           (home-directory "/var/empty")
           (shell (file-append shadow "/sbin/nologin"))))))

(define (tandoor-activation config)
  "Return an activation gexp for Tandoor."
  (let ((runtime (oci-tandoor-configuration-runtime config))
        (datadir (oci-tandoor-configuration-datadir config)))
    (if (string? datadir)
        #~(begin
            (use-modules (guix build utils))
            (let* ((user (getpwnam
                          (if #$(eq? 'podman runtime
                                     "oci-container" "tandoor"))))
                   (uid (passwd:uid user))
                   (gid (passwd:gid user))
                   (datadir #$datadir))
              ;; Setup datadir
              (mkdir-p datadir)
              (chown datadir uid gid)
              (chmod datadir #o660)))
        #~(begin))))

(define oci-tandoor-configuration->oci-container-configuration
  (lambda (config)
    (let* ((maybe-mediadir
            (oci-tandoor-configuration-mediadir config))
           (maybe-staticdir
            (oci-tandoor-configuration-staticdir config))
           (staticdir
            (if (maybe-value-set? maybe-datadir)
                maybe-datadir
                "/opt/recipes/staticfiles"))
           (mediadir
            (if (maybe-value-set? maybe-datadir)
                maybe-datadir
                "/opt/recipes/mediafiles"))
           (environment
            (tandoor-configuration->oci-container-environment
             tandoor-config))
           (extra-variables
            (oci-tandoor-configuration-extra-variables config))
           (log-file
            (oci-tandoor-configuration-log-file config))
           (network
            (oci-tandoor-configuration-network config))
           (image
            (oci-tandoor-configuration-image config))
           (requirement
            (oci-tandoor-configuration-requirement config))
           (port
            (oci-tandoor-configuration-port config))
           (secrets-directories
            (delete-duplicates
             (map (lambda (secret-file)
                    (define secret-directory (dirname secret-file))
                    (string-append secret-directory ":"
                                   secret-directory ":ro"))
                  (%tandoor-secrets-files config))))
           (container-config
            (mainline:oci-container-configuration
             (image image)
             (requirement `(,@requirement sops-secrets))
             (log-file log-file)
             (entrypoint "/bin/sh")
             (command
              `("-c" ,(oci-tandoor-sh-command
                       (%tandoor-secrets-specs config)
                       "/opt/recipes/boot.sh")))
             (environment
              (append
               environment
               '("LANG")
               extra-variables))
             (ports
              `((,port . "8080")))
             (volumes
              `((,(if (string? staticdir)
                      staticdir
                      (oci-volume-configuration-name staticdir))
                 . "/opt/recipes/staticfiles")
                (,(if (string? mediadir)
                      mediadir
                      (oci-volume-configuration-name mediadir))
                 . "/opt/recipes/mediafiles"))))))
      (list
       (if (maybe-value-set? network)
           (mainline:oci-container-configuration
            (inherit container-config)
            (ports '())
            (network network))
           container-config)))))

(define oci-tandoor-service-type
  (service-type (name 'tandoor)
                (extensions
                 (list
                  (service-extension oci-service-type
                                     (lambda (config)
                                       (oci-extension
                                        (volumes
                                         (let ((mediadir
                                                (oci-tandoor-configuration-mediadir config))
                                               (staticdir
                                                (oci-tandoor-configuration-staticdir config)))
                                           `(,@(if (oci-volume-configuration? mediadir) (list mediadir) '())
                                             ,@(if (oci-volume-configuration? staticdir) (list staticdir) '()))))
                                        (containers
                                         (oci-tandoor-configuration->oci-container-configuration config)))))
                  (service-extension account-service-type
                                     tandoor-accounts)
                  (service-extension sops-secrets-service-type
                                     (lambda (config)
                                       (%tandoor-secrets config)))
                  (service-extension postgresql-role-service-type
                                     (lambda (oci-config)
                                       (define config
                                         (oci-tandoor-configuration-configuration oci-config))
                                       (if (tandoor-configuration-create-database? config)
                                           (list
                                            (postgresql-role
                                             (name (tandoor-configuration-postgres-db config))
                                             (password-file
                                              (%tandoor-secrets-postgres-password-file config))
                                             (create-database? #t)))
                                           '())))
                  (service-extension activation-service-type
                                     tandoor-activation)))
                (description
                 "This service install a OCI backed Tandoor Shepherd Service.")))
