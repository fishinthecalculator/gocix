;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (oci services tandoor)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services databases)
  #:use-module ((gnu services docker) #:prefix mainline:)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:use-module (sops secrets)
  #:use-module (sops utils)
  #:use-module (sops services sops)
  #:use-module (oci services configuration)
  #:use-module (oci services containers)
  #:export (oci-tandoor-service-type

            tandoor-configuration
            tandoor-configuration?
            tandoor-configuration-fields
            tandoor-configuration-create-database?
            tandoor-configuration-db-engine
            tandoor-configuration-email-host
            tandoor-configuration-email-port
            tandoor-configuration-email-host-user
            tandoor-configuration-email-host-password-file
            tandoor-configuration-email-use-tls?
            tandoor-configuration-email-use-ssl?
            tandoor-configuration-default-from-email
            tandoor-configuration-postgres-db
            tandoor-configuration-postgres-host
            tandoor-configuration-postgres-user

            oci-tandoor-configuration
            oci-tandoor-configuration?
            oci-tandoor-configuration-fields
            oci-tandoor-configuration-runtime
            oci-tandoor-configuration-staticdir
            oci-tandoor-configuration-mediadir
            oci-tandoor-configuration-provision
            oci-tandoor-configuration-requirement
            oci-tandoor-configuration-image
            oci-tandoor-configuration-port
            oci-tandoor-configuration-secrets-directory
            oci-tandoor-configuration-postgres-password
            oci-tandoor-configuration-email-host-password
            oci-tandoor-configuration-secret-key
            oci-tandoor-configuration-log-file
            oci-tandoor-configuration-network
            oci-tandoor-configuration->oci-container-configuration

            oci-tandoor-provision
            oci-tandoor-log-file
            oci-tandoor-staticdir
            oci-tandoor-mediadir

            tandoor-accounts
            tandoor-activation))

;; Some of this code comes from the Guix manual.
;; Check it out! It's pretty cool.

(define tandoor-tag
  "1.5.34-open-data-plugin")

(define tandoor-image
  (string-append "docker.io/vabene1111/recipes:" tandoor-tag))

(define-maybe string)

(define (string-or-volume? value)
  (or (string? value)
      (oci-volume-configuration? value)))
(define-maybe/no-serialization string-or-volume)

(define-configuration/no-serialization tandoor-configuration
  (email-host
   (maybe-string)
   "The SMTP server used by Tandoor to send emails.")
  (email-port
   (maybe-string)
   "The SMTP port used by Tandoor to send emails.")
  (email-host-user
   (maybe-string)
   "The SMTP user used by Tandoor to authenticate against the email host.")
  (email-host-password-file
   (maybe-string)
   "The path of a the password file used by Tandoor to read the email password.")
  (email-use-tls?
   (boolean #f)
   "Whether Tandoor should use TLS to connect to the email server.")
  (email-use-ssl?
   (boolean #f)
   "Whether Tandoor should use SSL to connect to the email server.")
  (default-from-email
   (maybe-string)
   "The email address used in the From field of emails sent by Tandoor.")
  (create-database?
   (boolean #t)
   "Whether to create a database with the same name as the role.")
  (postgres-host
   (string "localhost")
   "The hostname where postgres will be looked for.")
  (db-engine
   (string "django.db.backends.postgresql")
   "The database engine used by Tandoor.  It defaults to
@code{django.db.backends.postgresql}.  You can look at
the @code{/opt/recipes/boot.sh} file inside the container
for more details.")
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

(define-maybe/no-serialization sops-secret)

(define-configuration/no-serialization oci-tandoor-configuration
  (runtime
   (symbol 'docker)
   "The OCI runtime to be used for this service.")
  (provision
   (maybe-string)
   "The name of the provisioned Shepherd service.  When unset, it defaults to
either @code{docker-tandoor} or @code{podman-tandoor} depending on the value of
the @code{runtime} field.")
  (staticdir
   (maybe-string-or-volume)
   "The directory where tandoor writes static files.  It can be either an
@code{oci-volume-configuration} representing the OCI volume where Tandoor will
write, or a string representing a file system path in the host system which
will be mapped inside the container.  By default it is @code{\"/var/lib/tandoor/staticfiles\"}.")
  (mediadir
   (maybe-string-or-volume)
   "The directory where tandoor writes media files.  It can be either an
@code{oci-volume-configuration} representing the OCI volume where Tandoor will
write, or a string representing a file system path in the host system which
will be mapped inside the container.  By default it is @code{\"/var/lib/tandoor/mediafiles\"}.")
  (configuration
   (tandoor-configuration (tandoor-configuration))
   "A tandoor-configuration record used to configure the Tandoor instance.")
  (log-file
   (maybe-string)
   "When @code{log-file} is set, it names the file to which the service’s
standard output and standard error are redirected.  @code{log-file} is created
if it does not exist, otherwise it is appended to.  By default it is
@code{\"/var/log/tandoor.log\"}.")
  (image
   (string tandoor-image)
   "The image to use for the OCI backed Shepherd service.")
  (port
   (string "8080")
   "This host port will be mapped onto the Tandoor configured port inside the container.")
  (requirement
   (list '(postgresql postgres-roles))
   "A list of Shepherd services that will be waited for before starting Tandoor.
The @code{sops-secrets} service is always appended to this list.")
  (secrets-directory
   (string "/run/secrets")
   "The directory where secrets are looked for.")
  (postgres-password
   (sops-secret)
   "POSTGRES_PASSWORD Tandoor secret.")
  (email-host-password
   (maybe-sops-secret)
   "EMAIL_HOST_PASSWORD Tandoor secret.")
  (secret-key
   (sops-secret)
   "SECRET_KEY Tandoor secret.")
  (network
   (maybe-string)
   "The docker network where the tandoor container will be attached. When equal
to \"host\" the @code{port} field will be ignored.")
  (extra-variables
   (list '())
   "A list of pairs representing any extra environment variable that should be
set inside the container. Refer to the
@uref{mainline, https://docs.tandoor.dev/install/docker/#docker} documentation
for more details."))

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
                                                            '()))
                                         #:true-value "1"
                                         #:false-value "0")))

(define (%tandoor-secrets config)
  (let ((email-host-password
         (oci-tandoor-configuration-email-host-password config)))
    `(,(oci-tandoor-configuration-postgres-password config)
      ,(oci-tandoor-configuration-secret-key config)
      ,@(if (maybe-value-set? email-host-password)
            (list email-host-password)
            '()))))

(define (%tandoor-secrets-variables config)
  `("POSTGRES_PASSWORD"
    "SECRET_KEY"
    ,@(if (maybe-value-set?
           (oci-tandoor-configuration-email-host-password config))
          '("EMAIL_HOST_PASSWORD")
          '())))

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
  (zip (%tandoor-secrets-variables config)
       (%tandoor-secrets-files config)))

(define (oci-tandoor-provision config)
  (define runtime-name
    (symbol->string
     (oci-tandoor-configuration-runtime config)))
  (define maybe-provision
    (oci-tandoor-configuration-provision config))
  (if (maybe-value-set? maybe-provision)
      maybe-provision
      (string-append runtime-name "-tandoor")))

(define (oci-tandoor-log-file config)
  (define maybe-log-file
    (oci-tandoor-configuration-log-file config))
  (if (maybe-value-set? maybe-log-file)
      maybe-log-file
      "/var/log/tandoor.log"))

(define (oci-tandoor-staticdir config)
  (define maybe-staticdir
    (oci-tandoor-configuration-staticdir config))
  (if (maybe-value-set? maybe-staticdir)
      maybe-staticdir
      "/var/lib/tandoor/staticfiles"))

(define (oci-tandoor-mediadir config)
  (define maybe-mediadir
    (oci-tandoor-configuration-mediadir config))
  (if (maybe-value-set? maybe-mediadir)
      maybe-mediadir
      "/var/lib/tandoor/mediafiles"))

(define (tandoor-accounts config)
  (let ((runtime (oci-tandoor-configuration-runtime config)))
    (list (user-account
           (name "tandoor")
           (comment "Tandoor's Service Account")
           (group (if (eq? 'podman runtime) "users" "root"))
           (supplementary-groups '("tty"))
           (system? (eq? 'docker runtime))
           (home-directory "/var/empty")
           (shell (file-append shadow "/sbin/nologin"))))))

(define (tandoor-activation config)
  "Return an activation gexp for Tandoor."
  (let* ((runtime (oci-tandoor-configuration-runtime config))
         (mediadir (oci-tandoor-mediadir config))
         (staticdir (oci-tandoor-staticdir config))
         (host-directories
          (filter string? (list mediadir staticdir))))
    #~(begin
        (use-modules (guix build utils))
        #$@(map (lambda (dir)
                  #~(let* ((user (getpwnam
                                  (if #$(eq? 'podman runtime)
                                      "oci-container" "tandoor")))
                           (uid (passwd:uid user))
                           (gid (passwd:gid user))
                           (dir #$dir))
                      ;; Setup datadir
                      (mkdir-p dir)
                      (chown dir uid gid)
                      ;; Somehow NGINX needs executable permissions
                      ;; to be able to serve static files.
                      (chmod dir #o755)))
                (if (any (lambda (d) (string-prefix? "/var/lib/tandoor" d))
                         host-directories)
                    (append '("/var/lib/tandoor") host-directories)
                    host-directories)))))

(define oci-tandoor-configuration->oci-container-configuration
  (lambda (config)
    (let* ((mediadir (oci-tandoor-mediadir config))
           (staticdir (oci-tandoor-staticdir config))
           (provision (oci-tandoor-provision config))
           (log-file (oci-tandoor-log-file config))
           (tandoor-config
            (oci-tandoor-configuration-configuration config))
           (environment
            (tandoor-configuration->oci-container-environment
             tandoor-config))
           (extra-variables
            (oci-tandoor-configuration-extra-variables config))
           (network
            (oci-tandoor-configuration-network config))
           (image
            (oci-tandoor-configuration-image config))
           (requirement
            (oci-tandoor-configuration-requirement config))
           (port
            (oci-tandoor-configuration-port config))
           (secrets
            (delete-duplicates
             (map (lambda (secret-file)
                    (string-append secret-file ":" secret-file ":ro"))
                  (%tandoor-secrets-files config))))
           (container-config
            (mainline:oci-container-configuration
             (provision provision)
             (image image)
             (requirement `(,@requirement sops-secrets))
             (log-file log-file)
             (entrypoint "/bin/sh")
             (command
              `("-c" ,(sops-secrets-sh-command-wrapper
                       (%tandoor-secrets-specs config)
                       ;; https://hub.docker.com/layers/vabene1111/recipes/1.5-open-data-plugin/images/sha256-821dbb6047ead52f981b05f6ac3411702d2881a8fb6c8c532ce4f653426d31c6
                       '("/opt/recipes/boot.sh"))))
             (environment
              (append
               environment
               '("LANG")
               extra-variables))
             (ports
              `((,port . "8080")))
             (volumes
              `(,@secrets
                (,(if (string? staticdir)
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
                                                (oci-tandoor-mediadir config))
                                               (staticdir
                                                (oci-tandoor-staticdir config)))
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
                                              (%tandoor-secrets-postgres-password-file oci-config))
                                             (create-database? #t)))
                                           '())))
                  (service-extension activation-service-type
                                     tandoor-activation)))
                (description
                 "This service install a OCI backed Tandoor Shepherd Service.")))
