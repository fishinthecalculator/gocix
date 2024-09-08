;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (oci services pict-rs)
  ;; #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services docker)
  #:use-module (gnu system shadow)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (ice-9 match)
  #:export (oci-pict-rs-configuration
            oci-pict-rs-configuration?
            oci-pict-rs-configuration-fields
            ;; oci-pict-rs-configuration-image
            ;; oci-pict-rs-configuration-port
            ;; oci-pict-rs-configuration-datadir
            ;; oci-pict-rs-configuration-database-path
            ;; oci-pict-rs-configuration-network
            ;; oci-pict-rs-configuration-extra-variables
            oci-pict-rs-configuration->oci-container-configuration
            oci-pict-rs-service-type))

;; Some of this code comes from the Guix manual.
;; Check it out! It's pretty cool.

(define pict-rs-tag
  "v1.6.0")

(define pict-rs-image
  (string-append "docker.io/asonix/pictrs:" pict-rs-tag))

(define-maybe string)

(define-configuration/no-serialization oci-pict-rs-configuration
  (image
   (string pict-rs-image)
   "The image to use for the OCI backed Shepherd service.")
  (port
   (string "8080")
   "The port where pict-rs will be exposed.")
  (datadir
   (string "/var/lib/pict-rs")
   "The directory where pict-rs writes state.")
  (network
   (maybe-string)
   "The docker network where the pict-rs container will be attached. When equal
to \"host\" the @code{port} field will not be mapped into the container's one.")
  (extra-variables
   (list '())
   "A list of pairs representing any extra environment variable that should be set inside the container. Refer to the @uref{upstream, https://git.asonix.dog/asonix/pict-rs} documentation for more details."))

(define (%pict-rs-activation config)
  "Return an activation gexp for pict-rs."
  (when config
    (let* ((datadir (oci-pict-rs-configuration-datadir config)))
      #~(begin
          (use-modules (guix build utils))
          (let* ((user (getpwnam "pict-rs"))
                 (uid (passwd:uid user))
                 (gid (passwd:gid user))
                 (datadir #$datadir))
            ;; Setup datadir
            (mkdir-p datadir)
            (chown datadir uid gid))))))

(define %pict-rs-accounts
  (list (user-group
         (name "pict-rs")
         (id 991)
         (system? #t))
        (user-account
          (name "pict-rs")
          (comment "pict-rs's Service Account")
          (uid 991)
          (group "pict-rs")
          (supplementary-groups '("tty"))
          (system? #t)
          (home-directory "/var/empty")
          (shell (file-append shadow "/sbin/nologin")))))

(define oci-pict-rs-configuration->oci-container-configuration
  (lambda (config)
    (when config
      (let* ((datadir
              (oci-pict-rs-configuration-datadir config))
             (extra-variables
              (oci-pict-rs-configuration-extra-variables config))
             (network
              (oci-pict-rs-configuration-network config))
             (image
              (oci-pict-rs-configuration-image config))
             (port
              (oci-pict-rs-configuration-port config))
             (container-config
              (oci-container-configuration
               (image image)
               (environment extra-variables)
               (ports
                `((,port . ,port)))
               (volumes
                `((,datadir . "/mnt"))))))
        (list
         (if (maybe-value-set? network)
             (oci-container-configuration
              (inherit container-config)
              (ports '())
              (network network))
             container-config))))))

(define oci-pict-rs-service-type
  (service-type (name 'pict-rs)
                (extensions (list (service-extension oci-container-service-type
                                                     oci-pict-rs-configuration->oci-container-configuration)
                                  (service-extension account-service-type
                                                     (const %pict-rs-accounts))
                                  (service-extension activation-service-type
                                                     %pict-rs-activation)))
                (default-value #f)
                (description
                 "This service install a OCI backed Pict-Rs Shepherd Service.")))
