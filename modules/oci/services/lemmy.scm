;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Giacomo Leidi <therewasa@fishinthecalculator.me>

(define-module (oci services lemmy)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services docker)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (sops secrets)
  #:use-module (sops services sops)
  #:use-module (oci build utils)
  #:use-module (oci self)
  #:use-module (oci services configuration)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (lemmy-database-configuration
            lemmy-database-configuration?
            lemmy-database-configuration-fields
            lemmy-database-configuration-user
            lemmy-database-configuration-password
            lemmy-database-configuration-host
            lemmy-database-configuration-port
            lemmy-database-configuration-database
            lemmy-database-configuration-pool-size
            lemmy-database-configuration-extra-content

            lemmy-pictrs-configuration
            lemmy-pictrs-configuration?
            lemmy-pictrs-configuration-fields
            lemmy-pictrs-configuration-url
            lemmy-pictrs-configuration-api-key
            lemmy-pictrs-configuration-extra-content

            lemmy-email-configuration
            lemmy-email-configuration?
            lemmy-email-configuration-fields
            lemmy-email-configuration-tls-type
            lemmy-email-configuration-smtp-from-address
            lemmy-email-configuration-smtp-server
            lemmy-email-configuration-smtp-login
            lemmy-email-configuration-smtp-password
            lemmy-email-configuration-extra-content

            lemmy-prometheus-configuration
            lemmy-prometheus-configuration?
            lemmy-prometheus-configuration-fields
            lemmy-prometheus-configuration-bind
            lemmy-prometheus-configuration-port
            lemmy-prometheus-configuration-extra-content

            lemmy-configuration
            lemmy-configuration?
            lemmy-configuration-fields
            lemmy-configuration-hostname
            lemmy-configuration-bind
            lemmy-configuration-port
            lemmy-configuration-tls-enabled?
            lemmy-configuration-worker-count
            lemmy-configuration-retry-count
            lemmy-configuration-database
            lemmy-configuration-pictrs
            lemmy-configuration-email
            lemmy-configuration-prometheus
            lemmy-configuration-extra-content


            oci-lemmy-configuration
            oci-lemmy-configuration?
            oci-lemmy-configuration-fields
            oci-lemmy-configuration-image
            oci-lemmy-configuration-configuration
            oci-lemmy-configuration-enable-lemmy-ui?
            oci-lemmy-configuration-extra-themes-directory
            oci-lemmy-configuration-lemmy-ui-port
            oci-lemmy-configuration-requirement
            oci-lemmy-configuration-log-file
            oci-lemmy-configuration-network
            oci-lemmy-configuration-auto-start?
            oci-lemmy-configuration-secrets-directory

            oci-lemmy-configuration->oci-container-configuration
            oci-lemmy-service-type))

(define lemmy-tag
  (string-append "0.19.5"))

(define lemmy-image
  (string-append "docker.io/dessalines/lemmy:" lemmy-tag))

(define (serialize-string field-name value)
  (serialize-hjson-string field-name value))

(define-maybe string)

;; From https://join-lemmy.org/docs/administration/configuration.html

(define-configuration/no-serialization lemmy-database-configuration
  (user
   (string)
   "Username to connect to postgres.")
  (password
   (sops-secret)
   "Password to connect to postgres.")
  (host
   (string)
   "Host where postgres is running.")
  (port
   (number)
   "Port where postgres can be accessed.")
  (database
   (string)
   "Name of the postgres database for Lemmy.")
  (pool-size
   (number 5)
   "Maximum number of active sql connections.")
  (extra-content
   (string "")
   "Any extra content you may want to add in the @uref{https://hjson.github.io, hjson} format."))

(define-configuration/no-serialization lemmy-pictrs-configuration
  (url
   (string "http://localhost:8080/")
   "Address where pictrs is available (for image hosting)")
  (api-key
   (sops-secret)
   "Set a custom pictrs API key (required for deleting images).")
  (extra-content
   (string "")
   "Any extra content you may want to add in the @uref{https://hjson.github.io, hjson} format."))

(define-configuration/no-serialization lemmy-email-configuration
  (tls-type
   (symbol 'none)
   "Whether or not smtp connections should use tls. Can be @code{none}, @code{tls}, or @code{starttls}.")
  (smtp-from-address
   (string "noreply@example.com")
   "Address to send emails from, eg \"noreply@@your-instance.com\".")
  (smtp-server
   (string "localhost:25")
   "Hostname and port of the smtp server.")
  (smtp-login
   (string "localhost:25")
   "Hostname and port of the smtp server.")
  (smtp-password
   (sops-secret)
   "The Lemmy secret used to login to the SMTP server.")
  (extra-content
   (string "")
   "Any extra content you may want to add in the @uref{https://hjson.github.io, hjson} format."))

(define-configuration/no-serialization lemmy-prometheus-configuration
  (bind
   (string "127.0.0.1")
   "The IP address or hostname of Prometheus.")
  (port
   (number 10002)
   "The port where Prometheus is listening for connections.")
  (extra-content
   (string "")
   "Any extra content you may want to add in the @uref{https://hjson.github.io, hjson} format."))

(define-configuration/no-serialization lemmy-configuration
  (hostname
   (string)
   "The domain name of your instance.")
  (bind
   (string "0.0.0.0")
   "Address where Lemmy should listen for incoming requests.")
  (port
   (number 8536)
   "Port where lemmy should listen for incoming requests.")
  (tls-enabled?
   (boolean #t)
   "Whether the site is available over TLS. Needs to be true for federation to work.")
  (worker-count
   (number 0)
   "The number of activitypub federation workers that can be in-flight concurrently
  worker_count: 0.")
  (retry-count
   (number 0)
   "The number of activitypub federation retry workers that can be in-flight concurrently.")
  (database
   (lemmy-database-configuration)
   "Settings related to the postgresql database.")
  (pictrs
   (lemmy-pictrs-configuration)
   "Pictrs image server configuration.")
  (email
   (lemmy-email-configuration)
   "Email sending configuration.")
  (prometheus
   (lemmy-prometheus-configuration)
   "Settings related to Prometheus.")
  (extra-content
   (string "")
   "Any extra content you may want to add in the @uref{https://hjson.github.io, hjson} format."))

(define-configuration/no-serialization oci-lemmy-configuration
  (image
   (string lemmy-image)
   "The image to use for the OCI backed Shepherd service.")
  (configuration
   (lemmy-configuration)
   "A lemmy-configuration record used to configure the Lemmy instance.")
  (enable-lemmy-ui?
   (boolean #t)
   "Whether to automatically install and enable Lemmy UI.  If set to @code{#f}, it can allow third party UIs to be used instead of Lemmy UI.")
  (extra-themes-directory
   (string "/var/lib/lemmy/ui/extra-themes")
   "The path where Lemmy UI will look for extra themes.  If @code{enable-lemmy-ui?} is false this field's value is ignored.")
  (lemmy-ui-port
   (number 1234)
   "Host port where lemmy-ui's port will be mapped.  If @code{enable-lemmy-ui?} is false this field's value is ignored.")
  (requirement
   (list '(docker-pictrs))
   "A list of Shepherd services that will be waited for before starting Lemmy's backend.")
  (rust-log
   (string "warn")
   "The value of @code{RUST_LOG} environment variable inside Lemmy's backend container.")
  (log-file
   (maybe-string)
   "When @code{log-file} is set, it names the file to which the service’s
standard output and standard error are redirected.  @code{log-file} is created
if it does not exist, otherwise it is appended to.")
  (network
   (maybe-string)
   "The docker network where the grafana container will be attached. When equal
to \"host\" the @code{port} field will be ignored.")
  (auto-start?
   (boolean #t)
   "Whether Lemmy should be started automatically by the Shepherd.  If it
is @code{#f} Lemmy has to be started manually with @command{herd start}.")
  (secrets-directory
   (string "/run/secrets")
   "The directory where secrets are looked for."))

(define (serialize-lemmy-config config)
  (string-append "{\n"
                 (configuration->hjson-block config lemmy-configuration-fields
                                             #:indentation "  "
                                             #:excluded '(extra-content)
                                             #:excluded-types (list sops-secret?))
                 "}\n"))

(define (%lemmy-secrets config)
  (define lemmy-config (oci-lemmy-configuration-configuration config))
  (list (lemmy-database-configuration-password
         (lemmy-configuration-database config))
        (lemmy-email-configuration-smtp-password
         (lemmy-configuration-email config))
        (lemmy-pictrs-configuration-api-key
         (lemmy-configuration-pictrs config))))

(define %lemmy-secrets-variables
  '("LEMMY_DATABASE_URL"
    "LEMMY_SMTP_PASSWORD"
    "LEMMY_PICTRS_API_KEY"))

(define (%lemmy-secrets-files config)
  (map (lambda (s)
         (string-append (oci-lemmy-configuration-secrets-directory config)
                        "/" (sops-secret->file-name s)))
       (%lemmy-secrets config)))

(define (%lemmy-secrets-specs config)
  (zip %lemmy-secrets-variables
       (%lemmy-secrets-files config)))

(define (%lemmy-activation config)
  "Return an activation gexp for Lemmy."
  (let* ((log-file
          (oci-lemmy-configuration-log-file config))
         (enable-lemmy-ui?
          (oci-lemmy-configuration-enable-lemmy-ui? config))
         (extra-themes-directory
          (oci-lemmy-configuration-extra-themes-directory config)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((user (getpwnam "lemmy"))
               (uid (passwd:uid user))
               (gid (passwd:gid user))
               (extra-themes-directory #$extra-themes-directory))
          ;; Setup datadirs
          (when #$enable-lemmy-ui?
            (mkdir-p extra-themes-directory)
            (chown extra-themes uid gid))

          (when #$(maybe-value-set? log-file)
                (let ((logs-directory (dirname #$log-file)))
                  (unless (file-exists? logs-directory)
                    (mkdir-p logs-directory))))))))

(define* (oci-lemmy-sh-command db-config secrets-specs command)
  "Exports each one of the SECRETS-SPECS as an environment variable
and returns Lemmy's sh command."
  (define user
    (lemmy-database-configuration-user db-config))
  (define database
    (lemmy-database-configuration-database db-config))
  (define host
    (lemmy-database-configuration-host db-config))
  (define port
    (lemmy-database-configuration-port db-config))
  (string-join
   `("set -e"
     ,@(map (match-lambda
              ((variable secret)
               (if (string=? variable "LEMMY_DATABASE_URL")
                   (string-append
                    ;; postgres://lemmy:password@lemmy_db:5432/lemmy
                    "export LEMMY_DATABASE_URL=\"" user ":$(cat " secret ")@" host ":" port "/" database "\"")
                   (string-append
                    "export " variable "=\"$(cat " secret ")\""))))
            secrets-specs)
     ,(string-append "exec " command))
   "; "))

(define oci-lemmy-configuration->oci-container-configuration
  (lambda (config)
    (let* ((auto-start?
            (oci-lemmy-configuration-auto-start? config))
           (lemmy-config
            (oci-lemmy-configuration-configuration config))
           (enable-lemmy-ui?
            (oci-lemmy-configuration-enable-lemmy-ui? config))
           (extra-themes-directory
            (oci-lemmy-configuration-extra-themes-directory config))
           (rust-log
            (oci-lemmy-configuration-rust-log config))
           (log-file
            (oci-lemmy-configuration-log-file config))
           (network
            (oci-lemmy-configuration-network config))
           (port
            (lemmy-configuration-port lemmy-config))
           (lemmy-ui-port
            (oci-lemmy-configuration-lemmy-ui-port config))
           (image
            (oci-lemmy-configuration-image config))
           (requirement
            (oci-lemmy-configuration-requirement config))
           (secrets-directories
            (secrets-volume-mappings
             (%lemmy-secrets-files config)))
           (frontend-container
            (oci-container-configuration
             (image (string-append "docker.io/dessalines/lemmy:" lemmy-tag))
             (provision "lemmy-ui")
             (requirement '(lemmy))
             (environment
              `(("LEMMY_UI_LEMMY_INTERNAL_HOST" .
                 ,(string-append
                   (if (and (maybe-value-set? network)
                            (string=? network "host"))
                       "localhost" "lemmy") ":" port))
                ("LEMMY_UI_LEMMY_EXTERNAL_HOST" .
                 ,(lemmy-configuration-hostname lemmy-config))
                ("LEMMY_UI_HTTPS" . "true")))
             (volumes
              `((,extra-themes-directory . "/app/extra_themes")))
             (ports
              (if enable-lemmy-ui?
                  `((,lemmy-ui-port . "1234"))
                  '()))))
           (backend-container
            (oci-container-configuration
             (image image)
             (provision "lemmy")
             (auto-start? auto-start?)
             (requirement `(,@requirement sops-secrets postgres))
             (environment
              `("LANG"
                ("RUST_LOG" . ,rust-log)))
             (entrypoint "/bin/sh")
             (command
              `("-c" ,(oci-lemmy-sh-command (lemmy-configuration-database config)
                                            (%lemmy-secrets-specs config)
                                            "/usr/local/bin/lemmy_server")))
             (ports
              `((,port . ,port)))
             (volumes
              `((,(serialize-lemmy-config lemmy-config) . "/config/config.hjson")
                ,@secrets-directories))
             (log-file log-file))))
      (let ((backend
             (list
              (if (maybe-value-set? network)
                  (oci-container-configuration
                   (inherit backend-container)
                   (ports '())
                   (network network))
                  backend-container))))
        (if enable-lemmy-ui?
            (cons
             (if (maybe-value-set? network)
                 (oci-container-configuration
                  (inherit frontend-container)
                  (ports '())
                  (network network))
                 frontend-container)
             backend)
            backend)))))

(define %lemmy-accounts
  (list (user-account
          (name "lemmy")
          (comment "Lemmy's Service Account")
          (group "users")
          (supplementary-groups '("tty"))
          (system? #t)
          (home-directory "/var/empty")
          (shell (file-append shadow "/sbin/nologin")))))

(define oci-lemmy-service-type
  (service-type (name 'lemmy)
                (extensions (list (service-extension oci-container-service-type
                                                     oci-lemmy-configuration->oci-container-configuration)
                                  (service-extension sops-secrets-service-type
                                                     (lambda (config)
                                                       (%lemmy-secrets config)))
                                  (service-extension account-service-type
                                                     (const %lemmy-accounts))
                                  (service-extension activation-service-type
                                                     %lemmy-activation)))
                (description
                 "This service install a OCI backed Lemmy Shepherd Service.")))
