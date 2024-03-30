;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (oci services grafana)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 string-fun)
  #:use-module (oci services configuration)
  #:use-module (oci services docker)
  #:export (oci-grafana-service-type
            oci-grafana-configuration
            oci-grafana-configuration?
            oci-grafana-configuration-fields
            oci-grafana-configuration-datadir
            oci-grafana-configuration-image
            oci-grafana-configuration-port
            oci-grafana-configuration-grafana.ini
            oci-grafana-configuration-network
            oci-grafana-configuration->oci-container-configuration

            %grafana-accounts
            %grafana-activation

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
            grafana-smtp-configuration-from-address))

;; Some of this code comes from the Guix manual.
;; Check it out! It's pretty cool.

(define grafana-tag
  "10.1.5")

(define grafana-image
  (string-append "bitnami/grafana:" grafana-tag))

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

(define (gf-serialize-grafana-smtp-configuration field-name value)
  #~(string-append
     "[smtp]\n"
     #$(serialize-configuration
        value grafana-smtp-configuration-fields)))

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
   "The password used to authenticate with the SMTP server.")
  (from-address
   (string "alert@example.org")
   "The sender of the email alerts Grafana will send."))

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

(define-configuration oci-grafana-configuration
  (datadir
   (string "/var/lib/grafana")
   "The directory where grafana writes state.")
  (image
   (string grafana-image)
   "The image to use for the OCI backed Shepherd service.")
  (port
   (string "3000")
   "This host port will be mapped onto the Grafana configured port inside the container.")
  (grafana.ini
   (grafana-configuration (grafana-configuration))
   "This field will be serialized as graphana.ini.")
  (network
   (maybe-string)
   "The docker network where the grafana container will be attached. When equal
to \"host\" the @code{port} field will be ignored.")
  (no-serialization))

(define %grafana-accounts
  (list (user-account
         (name "grafana")
         (comment "Grafana's Service Account")
         (uid 1001)
         (group "root")
         (supplementary-groups '("tty"))
         (system? #t)
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (%grafana-activation config)
  "Return an activation gexp for Grafana."
  (let* ((datadir (oci-grafana-configuration-datadir config))
         (grafana.ini
          (mixed-text-file
           "grafana.ini"
           (serialize-configuration (oci-grafana-configuration-grafana.ini config)
                                    grafana-configuration-fields))))
    #~(begin
        (use-modules (guix build utils))
        (let* ((user (getpwnam "grafana"))
               (uid (passwd:uid user))
               (gid (passwd:gid user))
               (datadir #$datadir))
          ;; Setup datadir
          (mkdir-p datadir)
          (chown datadir uid gid)
          ;; Activate configuration
          (activate-special-files
           '(("/etc/grafana/grafana.ini"
              #$grafana.ini)))))))

(define oci-grafana-configuration->oci-container-configuration
  (lambda (config)
    (let* ((datadir
            (oci-grafana-configuration-datadir config))
           (grafana.ini (oci-grafana-configuration-grafana.ini config))
           (network
            (oci-grafana-configuration-network config))
           (image
            (oci-grafana-configuration-image config))
           (port
            (oci-grafana-configuration-port config))
           (container-config
            (oci-container-configuration
             (image image)
             (ports
              `((,port . "3000")))
             (volumes
              `((,datadir . "/var/lib/grafana")
                ;; Needed because grafana.ini is a symlink to an item in the store.
                ("/gnu/store" . "/gnu/store")
                ("/etc/grafana/grafana.ini" . "/opt/bitnami/grafana/conf/grafana.ini"))))))
      (list
       (if (maybe-value-set? network)
           (oci-container-configuration
            (inherit container-config)
            (network network))
           container-config)))))

(define oci-grafana-service-type
  (service-type (name 'grafana)
                (extensions (list (service-extension oci-container-service-type
                                                     oci-grafana-configuration->oci-container-configuration)
                                  (service-extension account-service-type
                                                     (const %grafana-accounts))
                                  (service-extension activation-service-type
                                                     %grafana-activation)))
                (default-value (oci-grafana-configuration))
                (description
                 "This service install a OCI backed Grafana Shepherd Service.")))
