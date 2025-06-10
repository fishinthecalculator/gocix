;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (oci services vaultwarden)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module ((gnu services docker) #:prefix mainline:)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (sops secrets)
  #:use-module (sops services sops)
  #:use-module (oci services configuration)
  #:use-module (oci services containers)
  #:use-module (srfi srfi-1)
  #:export (vaultwarden-configuration
            vaultwarden-configuration?
            vaultwarden-configuration-fields
            vaultwarden-configuration-domain
            vaultwarden-configuration-signups-allowed?

            oci-vaultwarden-configuration
            oci-vaultwarden-configuration?
            oci-vaultwarden-configuration-fields
            oci-vaultwarden-configuration-image
            oci-vaultwarden-configuration-data-directory
            oci-vaultwarden-configuration-port
            oci-vaultwarden-configuration-secrets-directory
            oci-vaultwarden-configuration-requirement
            oci-vaultwarden-configuration-auto-start?
            oci-vaultwarden-configuration-log-file
            oci-vaultwarden-configuration-network

            oci-vaultwarden-log-file

            oci-vaultwarden-service-type))

(define vaultwarden-version
  "1.33.2")

(define vaultwarden-image
  (string-append "docker.io/vaultwarden/server:" vaultwarden-version))

(define-maybe/no-serialization string)

(define-configuration/no-serialization vaultwarden-configuration
  (signups-allowed?
   (boolean #f)
   "This should be set to @code{#t} for the first run, and then deactivated
after you have created your account so that no strangers can register.")
  (domain
   (string)
   "The domain name where Vaultwarden will be exposed. Note that Vaultwarden
needs to know whether it's https to work properly with attachments"))

(define (string-or-volume? value)
  (or (string? value)
      (oci-volume-configuration? value)))
(define-maybe/no-serialization string-or-volume)

(define-configuration/no-serialization oci-vaultwarden-configuration
  (image
   (string vaultwarden-image)
   "The image to use for the OCI backed Shepherd service.")
  (data-directory
   (maybe-string-or-volume)
   "The directory where Vaultwarden writes uploaded files.  It can be either an
@code{oci-volume-configuration} representing the OCI volume where Vaultwarden will
write, or a string representing a file system path in the host system which
will be mapped inside the container.  By default it is @code{\"/var/lib/vaultwarden\"}.")
  (configuration
   (vaultwarden-configuration)
   "A @code{vaultwarden-configuration} record used to configure the Vaultwarden instance.")
  (requirement
   (list '(user-processes sops-secrets))
   "A list of Shepherd services that will be waited for before starting Vaultwarden.")
  (port
   (string "8080")
   "This host port will be mapped onto the Vaultwarden configured port inside the container.")
  (log-file
   (maybe-string)
   "When @code{log-file} is set, it names the file to which the service’s
standard output and standard error are redirected.  @code{log-file} is created
if it does not exist, otherwise it is appended to.  By default it is
@code{\"/var/log/vaultwarden.log\"}.")
  (auto-start?
   (boolean #t)
   "Whether Vaultwarden should be started automatically by the Shepherd.  If it
is @code{#f} Vaultwarden has to be started manually with @command{herd start}.")
  (secrets-directory
   (string "/run/secrets")
   "The directory where secrets are looked for.")
  (network
   (maybe-string)
   "The OCI network name where the vaultwarden container will be attached. When equal
to \"host\" the @code{port} field will not be mapped into the container's one.")
  (extra-variables
   (list '())
   "A list of pairs representing any extra environment variable that should be set inside the container."))

(define (oci-vaultwarden-log-file config)
  (define maybe-log-file
    (oci-vaultwarden-configuration-log-file config))
  (if (maybe-value-set? maybe-log-file)
      maybe-log-file
      "/var/log/vaultwarden.log"))

(define (oci-vaultwarden-data-directory config)
  (define maybe-data-directory
    (oci-vaultwarden-configuration-data-directory config))
  (if (maybe-value-set? maybe-data-directory)
      maybe-data-directory
      "/var/lib/vaultwarden"))

(define (vaultwarden-activation config)
  "Return an activation gexp for Vaultwarden."
  (let ((data-directory
         (oci-vaultwarden-data-directory config)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((data-directory #$data-directory)
               (user (getpwnam "oci-container"))
               (uid (passwd:uid user))
               (gid (passwd:gid user)))
          ;; Setup uploads directory.
          (mkdir-p data-directory)
          (chown data-directory uid gid)
          (chmod data-directory #o755)))))

(define oci-vaultwarden-configuration->oci-container-configuration
  (lambda (config)
    (let* ((auto-start?
            (oci-vaultwarden-configuration-auto-start? config))
           (data-directory
            (oci-vaultwarden-data-directory config))
           (extra-variables
            (oci-vaultwarden-configuration-extra-variables config))
           (environment
            (configuration->environment-variables
             config vaultwarden-configuration-fields))
           (log-file
            (oci-vaultwarden-log-file config))
           (network
            (oci-vaultwarden-configuration-network config))
           (port
            (oci-vaultwarden-configuration-port config))
           (image
            (oci-vaultwarden-configuration-image config))
           (requirement
            (oci-vaultwarden-configuration-requirement config))
           (container-config
            (mainline:oci-container-configuration
             (image image)
             (auto-start? auto-start?)
             (requirement requirement)
             (environment
              (append
               environment
               extra-variables))
             (ports
              `((,port . "80")))
             (volumes
              `((,(if (string? data-directory)
                      data-directory
                      (oci-volume-configuration-name data-directory))
                 . "/data")))
             (log-file log-file))))
      (list
       (if (string=? network "host")
           (mainline:oci-container-configuration
            (inherit container-config)
            (ports '())
            (network network))
           container-config)))))

(define oci-vaultwarden-service-type
  (service-type (name 'vaultwarden)
                (extensions (list (service-extension oci-service-type
                                                     (lambda (config)
                                                       (oci-extension
                                                        (volumes
                                                         (let ((data-directory
                                                                (oci-vaultwarden-data-directory config)))
                                                           `(,@(if (oci-volume-configuration? data-directory) (list data-directory) '()))))
                                                        (containers
                                                         (oci-vaultwarden-configuration->oci-container-configuration config)))))
                                  (service-extension activation-service-type
                                                     vaultwarden-activation)))
                (description
                 "This service install a OCI backed Vaultwarden Shepherd Service.")))
