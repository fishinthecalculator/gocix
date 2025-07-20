;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023, 2025 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (oci services grafana)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module ((gnu services docker) #:prefix mainline:)
  #:use-module (gnu system shadow)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (oci build utils)
  #:use-module (oci services configuration)
  #:use-module (oci services containers)
  #:use-module (sops services sops)
  #:use-module (sops secrets)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (oci-grafana-service-type
            oci-grafana-configuration
            oci-grafana-configuration?
            oci-grafana-configuration-fields
            oci-grafana-configuration-runtime
            oci-grafana-configuration-datadir
            oci-grafana-configuration-image
            oci-grafana-configuration-port
            oci-grafana-configuration-log-file
            oci-grafana-configuration-auto-start?
            oci-grafana-configuration-grafana.ini
            oci-grafana-configuration-network
            oci-grafana-configuration->oci-container-configuration

            oci-grafana-log-file

            grafana-accounts
            grafana-activation

            grafana-configuration
            grafana-configuration?
            grafana-configuration-fields
            grafana-configuration-server
            grafana-configuration-smtp
            grafana-configuration-extra-content

            grafana-server-configuration
            grafana-server-configuration?
            grafana-server-configuration-fields
            grafana-server-configuration-root-url
            grafana-server-configuration-serve-from-subpath?

            grafana-smtp-configuration
            grafana-smtp-configuration?
            grafana-smtp-configuration-fields
            grafana-smtp-configuration-enabled?
            grafana-smtp-configuration-host
            grafana-smtp-configuration-user
            grafana-smtp-configuration-password
            grafana-smtp-configuration-password-file
            grafana-smtp-configuration-from-address))

;; Some of this code comes from the Guix manual.
;; Check it out! It's pretty cool.

;; FIXME: Drop this and make this configurable.
(define %grafana-secrets-directory
  "/run/secrets")

(define grafana-tag
  "12.0.1")

(define grafana-image
  (string-append "docker.io/bitnami/grafana:" grafana-tag))

(define serialize-string serialize-ini-string)
(define serialize-integer serialize-ini-integer)
(define serialize-boolean serialize-ini-boolean)

(define (gf-serialize-grafana-server-configuration field-name value)
  #~(string-append
     "[server]\n"
     #$(serialize-configuration
        value grafana-server-configuration-fields)))

(define-configuration grafana-server-configuration
  (domain
   (string "example.org")
   "The public host where grafana will be exposed.")
  (root-url
   (string "%(protocol)s://%(domain)s:%(http_port)s/")
   "The url where grafana will be exposed.")
  (serve-from-sub-path?
   (boolean #f)
   "The image to use for the OCI backed Shepherd service."))

(define-maybe sops-secret)

(define-configuration grafana-smtp-configuration
  (enabled?
   (boolean #f)
   "Whether to enable Grafana's email alerting.")
  (host
   (string "smtp.example.org:587")
   "The connection string representing your SMTP server.")
  (user
   (string "you@example.org")
   "The email used to authenticate with the SMTP server.")
  (password
   (string "")
   "The password used to authenticate with the SMTP server."
   (serializer empty-serializer))
  (password-file
   (maybe-sops-secret)
   "An optional field representing a file from which Grafana
will read the SMTP password."
   (serializer empty-serializer))
  (from-address
   (string "alert@example.org")
   "The sender of the email alerts Grafana will send."))

(define (gf-serialize-grafana-smtp-configuration field-name value)
  (define password-file (grafana-smtp-configuration-password-file value))
  (define password (grafana-smtp-configuration-password value))
  (define serialized-secret
    (if (maybe-value-set? password-file)
        (string-append "password = $__file{"
                       %grafana-secrets-directory "/"
                       (sops-secret->file-name password-file)
                       "}\n")
        (if (string-null? password)
            ""
            (string-append "password = " password "\n"))))
  #~(string-append
     "[smtp]\n"
     #$(serialize-configuration
        value grafana-smtp-configuration-fields)
     #$serialized-secret))

(define (gf-serialize-grafana-configuration configuration)
  (mixed-text-file
   "grafana.ini"
   (serialize-configuration
    configuration grafana-configuration-fields)))

(define (gf-serialize-string field-name value)
  value)

(define-maybe string)

(define-configuration grafana-configuration
  (server
   (grafana-server-configuration (grafana-server-configuration))
   "grafana.ini's [server] section.")
  (smtp
   (grafana-smtp-configuration (grafana-smtp-configuration))
   "grafana.ini's [smtp] section.")
  (extra-content
   (string "")
   "Everything you want to manually add to grafana.ini.")
  (prefix gf-))

(define (string-or-volume? value)
  (or (string? value)
      (oci-volume-configuration? value)))
(define-maybe/no-serialization string-or-volume)

(define-configuration/no-serialization oci-grafana-configuration
  (runtime
   (symbol 'docker)
   "The OCI runtime to be used for this service")
  (datadir
   (maybe-string-or-volume)
   "The directory where grafana writes state.  It can be either an
@code{oci-volume-configuration} representing the OCI volume where Grafana will
write state, or a string representing a file system path in the host system which
will be mapped inside the container.  By default it is @code{\"/var/lib/grafana\"}.")
  (image
   (string grafana-image)
   "The image to use for the OCI backed Shepherd service.")
  (port
   (string "3000")
   "This host port will be mapped onto the Grafana configured port inside the container.")
  (auto-start?
   (boolean #t)
   "Whether Grafana should be started automatically by the Shepherd.  If it
is @code{#f} Grafana has to be started manually with @command{herd start}.")
  (grafana.ini
   (grafana-configuration (grafana-configuration))
   "This field will be serialized as graphana.ini.")
  (log-file
   (maybe-string)
   "When @code{log-file} is set, it names the file to which the service’s
standard output and standard error are redirected.  @code{log-file} is created
if it does not exist, otherwise it is appended to.  By default it is
@code{\"/var/log/grafana.log\"}.")
  (network
   (maybe-string)
   "The docker network where the grafana container will be attached. When equal
to \"host\" the @code{port} field will be ignored."))

(define (oci-grafana-log-file config)
  (define maybe-log-file
    (oci-grafana-configuration-log-file config))
  (if (maybe-value-set? maybe-log-file)
      maybe-log-file
      "/var/log/grafana.log"))

(define (%grafana-secrets config)
  (define record (oci-grafana-configuration-grafana.ini config))
  (if (grafana-configuration? record)
   (let ((secret (grafana-smtp-configuration-password-file
                  (grafana-configuration-smtp record))))
     (if (sops-secret? secret)
         (list secret)
         '()))
   '()))

(define (%grafana-secret-file config secret)
  (string-append %grafana-secrets-directory
                 "/" (sops-secret->file-name secret)))

(define (%grafana-secrets-files config)
  (map (lambda (s) (%grafana-secret-file config s))
       (%grafana-secrets config)))

(define (oci-grafana-datadir config)
  (define maybe-datadir
    (oci-grafana-configuration-datadir config))
  (if (maybe-value-set? maybe-datadir)
      maybe-datadir
      "/var/lib/grafana"))

(define (oci-grafana-grafana.ini config)
  (mixed-text-file
   "grafana.ini"
   (serialize-configuration (oci-grafana-configuration-grafana.ini config)
                            grafana-configuration-fields)))

(define (grafana-accounts config)
  (let ((runtime (oci-grafana-configuration-runtime config)))
    (if (eq? runtime 'podman)
        '()
        (list (user-account
               (name "grafana")
               (comment "Grafana's Service Account")
               (uid 1001)
               (group "root")
               (supplementary-groups '("tty"))
               (system? #t)
               (home-directory "/var/empty")
               (shell (file-append shadow "/sbin/nologin")))))))

(define (grafana-activation config)
  "Return an activation gexp for Grafana."
  (let* ((datadir (oci-grafana-datadir config))
         (grafana.ini (oci-grafana-grafana.ini config))
         (runtime (oci-grafana-configuration-runtime config)))
    #~(begin
        (use-modules (guix build utils))
        #$(if (string? datadir)
              #~(let* ((user (getpwnam
                              (if #$(eq? 'podman runtime)
                                  "oci-container" "grafana")))
                       (uid (passwd:uid user))
                       (gid (passwd:gid user))
                       (datadir #$datadir))
                  ;; Setup datadir
                  (mkdir-p datadir)
                  (chown datadir uid gid)
                  (if #$(eq? 'podman runtime)
                      (chmod datadir #o700)
                      (chmod datadir #o755)))
              #~(begin))
        ;; Activate configuration
        (activate-special-files
         '(("/etc/grafana/grafana.ini"
            #$grafana.ini))))))

(define oci-grafana-configuration->oci-container-configuration
  (lambda (config)
    (let* ((auto-start?
            (oci-grafana-configuration-auto-start? config))
           (datadir (oci-grafana-datadir config))
           (grafana.ini (oci-grafana-grafana.ini config))
           (password-file
            (let ((maybe-record
                   (oci-grafana-configuration-grafana.ini config)))
              (and (grafana-configuration? maybe-record)
                   (grafana-smtp-configuration-password-file
                    (grafana-configuration-smtp maybe-record)))))
           (network
            (oci-grafana-configuration-network config))
           (image
            (oci-grafana-configuration-image config))
           (port
            (oci-grafana-configuration-port config))
           (log-file (oci-grafana-log-file config))
           (runtime (oci-grafana-configuration-runtime config))
           (secrets-directories
            (secrets-volume-mappings
             (%grafana-secrets-files config)
             #:mode (if (eq? 'podman runtime) "U" "ro")))
           (container-config
            (mainline:oci-container-configuration
             (auto-start? auto-start?)
             (log-file log-file)
             (requirement
              (if (and password-file (maybe-value-set? password-file))
                  '(sops-secrets)
                  '()))
             ;; HACK: required to map container user to `oci-container' host
             ;; user.
             (extra-arguments '("--userns=keep-id:uid=1001"))
             (image image)
             (ports
              `((,port . "3000")))
             (volumes
              `((,(if (string? datadir)
                      datadir
                      (oci-volume-configuration-name datadir))
                 . "/var/lib/grafana")
                ,@secrets-directories
                (,grafana.ini . "/opt/bitnami/grafana/conf/grafana.ini"))))))
      (list
       (if (maybe-value-set? network)
           (mainline:oci-container-configuration
            (inherit container-config)
            (ports '())
            (network network))
           container-config)))))

(define oci-grafana-service-type
  (service-type (name 'grafana)
                (extensions (list (service-extension oci-service-type
                                                     (lambda (config)
                                                       (oci-extension
                                                        (volumes
                                                         (let ((datadir (oci-grafana-datadir config)))
                                                           (if (oci-volume-configuration? datadir) (list datadir) '())))
                                                        (containers
                                                         (oci-grafana-configuration->oci-container-configuration config)))))
                                  (service-extension account-service-type
                                                     grafana-accounts)
                                  (service-extension sops-secrets-service-type
                                                     (lambda (config)
                                                       (define password-file
                                                         (let ((maybe-record
                                                                (oci-grafana-configuration-grafana.ini config)))
                                                           (and (grafana-configuration? maybe-record)
                                                                (grafana-smtp-configuration-password-file
                                                                 (grafana-configuration-smtp maybe-record)))))
                                                       (if (and password-file (maybe-value-set? password-file))
                                                           (%grafana-secrets config)
                                                           '())))
                                  (service-extension activation-service-type
                                                     grafana-activation)))
                (default-value (oci-grafana-configuration))
                (description
                 "This service install a OCI backed Grafana Shepherd Service.")))
