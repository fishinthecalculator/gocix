;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023, 2025 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (oci services forgejo)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module ((gnu services docker) #:prefix mainline:)
  #:use-module (gnu system shadow)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (oci services containers)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:export (oci-forgejo-configuration
            oci-forgejo-configuration?
            oci-forgejo-configuration-fields
            oci-forgejo-configuration-log-file
            oci-forgejo-configuration-uid
            oci-forgejo-configuration-gid
            oci-forgejo-configuration-image
            oci-forgejo-configuration-port
            oci-forgejo-configuration-ssh-port
            oci-forgejo-configuration-datadir
            oci-forgejo-configuration-app.ini

            forgejo-accounts
            forgejo-activation

            oci-forgejo-configuration->oci-container-configuration

            oci-forgejo-service-type))

;; Some of this code comes from the Guix manual.
;; Check it out! It's pretty cool.

(define forgejo-tag
  "10.0.1-rootless")

(define forgejo-image
  (string-append "codeberg.org/forgejo/forgejo:" forgejo-tag))

(define (string-or-volume? value)
  (or (string? value)
      (oci-volume-configuration? value)))

(define-maybe/no-serialization string-or-volume)
(define-maybe/no-serialization string)
(define-maybe/no-serialization file-like)

(define-configuration/no-serialization oci-forgejo-configuration
  (uid
   (positive 34595)
   "The uid assigned to the Forgejo service account.")
  (gid
   (positive 98715)
   "The gid assigned to the Forgejo service account.")
  (log-file
   (maybe-string)
   "When @code{log-file} is set, it names the file to which the service’s
standard output and standard error are redirected.  @code{log-file} is created
if it does not exist, otherwise it is appended to.  By default it is
@code{\"/var/log/forgejo.log\"}.")
  (runtime
   (symbol 'docker)
   "The OCI runtime to be used for this service")
  (image
   (string forgejo-image)
   "The image to use for the OCI backed Shepherd service.")
  (port
   (string "3000")
   "The port where forgejo will be exposed.")
  (ssh-port
   (string "2202")
   "The port where forgejo's ssh service will be exposed.")
  (datadir
   (maybe-string-or-volume)
   "The directory where forgejo writes state.  It can be either an
@code{oci-volume-configuration} representing the OCI volume where Forgejo will
write state, or a string representing a file system path in the host system which
will be mapped inside the container.  By default it is @code{\"/var/lib/forgejo\"}.")
  (network
   (maybe-string)
   "The OCI network where the forgejo container will be attached. When equal
to \"host\" the @code{port} field will be ignored.")
  (app.ini
   (maybe-file-like)
   "The @code{app.ini} configuration passed to Forgejo."))

(define (oci-forgejo-datadir config)
  (define maybe-datadir
    (oci-forgejo-configuration-datadir config))
  (if (maybe-value-set? maybe-datadir)
      maybe-datadir
      "/var/lib/forgejo"))

(define (oci-forgejo-log-file config)
  (define maybe-log-file
    (oci-forgejo-configuration-log-file config))
  (if (maybe-value-set? maybe-log-file)
      maybe-log-file
      "/var/log/forgejo.log"))

(define (forgejo-accounts config)
  (let ((runtime (oci-forgejo-configuration-runtime config)))
    (list (user-group
           (system? (eq? 'docker runtime))
           (name "forgejo")
           (id (oci-forgejo-configuration-gid config)))
          (user-account
           (name "forgejo")
           (comment "Forgejo's Service Account")
           (uid (oci-forgejo-configuration-uid config))
           (group (if (eq? 'podman runtime) "users" "forgejo"))
           (supplementary-groups '("tty"))
           (system? (eq? 'docker runtime))
           (home-directory "/var/empty")
           (shell (file-append shadow "/sbin/nologin"))))))

(define (forgejo-activation config)
  "Return an activation gexp for Forgejo."
  (let* ((runtime (oci-forgejo-configuration-runtime config))
         (datadir (oci-forgejo-datadir config))
         (log-file (oci-forgejo-log-file config))
         (gid
           (oci-forgejo-configuration-gid config))
         (uid
           (oci-forgejo-configuration-uid config)))
    #~(begin
        (use-modules (guix build utils))
        ;; Setup log directory
        (let ((logs-directory (dirname #$log-file)))
          (unless (file-exists? logs-directory)
            (mkdir-p logs-directory)))
        ;; Setup datadir
        #$(if (string? datadir)
              #~(let ((datadir #$datadir)
                      (uid
                       (if #$(eq? 'podman runtime)
                           (passwd:uid (getpwnam"oci-container"))
                           ;; FIXME: Forgejo seems to ignore USER_UID and USER_GID
                           1000))
                      (gid
                       (if #$(eq? 'podman runtime)
                           (passwd:gid (getpwnam"oci-container"))
                           ;; FIXME: Forgejo seems to ignore USER_UID and USER_GID
                           1000)))
                  ;; Setup datadir
                  (mkdir-p datadir)
                  (chown datadir uid gid)
                  (if #$(eq? 'podman runtime)
                      (chmod datadir #o660)
                      (chmod datadir #o750)))
              #~(begin)))))

(define oci-forgejo-configuration->oci-container-configuration
  (lambda (config)
    (let* ((app.ini (oci-forgejo-configuration-app.ini config))
           (datadir (oci-forgejo-datadir config))
           (log-file (oci-forgejo-log-file config))
           (gid
            (oci-forgejo-configuration-gid config))
           (image
            (oci-forgejo-configuration-image config))
           (network
            (oci-forgejo-configuration-network config))
           (port
            (oci-forgejo-configuration-port config))
           (ssh-port
            (oci-forgejo-configuration-ssh-port config))
           (uid
            (oci-forgejo-configuration-uid config))
           (container-config
            (mainline:oci-container-configuration
             (image image)
             (log-file log-file)
             (environment
              `(("USER_UID" . ,(number->string uid))
                ("USER_GID" . ,(number->string gid))))
             (ports
              `((,port . "3000")
                (,ssh-port . "22")))
             (volumes
              `((,(if (string? datadir)
                      datadir
                      (oci-volume-configuration-name datadir))
                 . "/var/lib/gitea")
                ,@(if (maybe-value-set? app.ini)
                      '((,app.ini . "/etc/gitea/app.ini:ro"))
                      '())
                ("/etc/timezone" . "/etc/timezone:ro")
                ("/etc/localtime" . "/etc/localtime:ro"))))))
      (list
       (if (maybe-value-set? network)
           (mainline:oci-container-configuration
            (inherit container-config)
            (ports '())
            (network network))
           container-config)))))

(define oci-forgejo-service-type
  (service-type (name 'forgejo)
                (extensions (list (service-extension oci-service-type
                                                     (lambda (config)
                                                       (oci-extension
                                                        (volumes
                                                         (let ((datadir (oci-forgejo-datadir config)))
                                                           (if (oci-volume-configuration? datadir) (list datadir) '())))
                                                        (containers
                                                         (oci-forgejo-configuration->oci-container-configuration config)))))
                                  (service-extension account-service-type
                                                     forgejo-accounts)
                                  (service-extension activation-service-type
                                                     forgejo-activation)))
                (default-value (oci-forgejo-configuration))
                (description
                 "This service install a OCI backed Forgejo Shepherd Service.")))
