;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (oci services forgejo)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu system shadow)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (oci services docker)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:export (forgejo-configuration
            forgejo-configuration?
            forgejo-configuration-fields
            forgejo-configuration-uid
            forgejo-configuration-gid
            forgejo-configuration-image
            forgejo-configuration-port
            forgejo-configuration-ssh-port
            forgejo-configuration-datadir
            forgejo-configuration-app.ini

            %forgejo-accounts
            %forgejo-activation

            forgejo-configuration->oci-container-configuration

            oci-forgejo-service-type))

;; Some of this code comes from the Guix manual.
;; Check it out! It's pretty cool.

(define forgejo-tag
  "1.21.4-0-rootless")

(define forgejo-image
  (string-append "codeberg.org/forgejo/forgejo:" forgejo-tag))

(define-maybe/no-serialization file-like)

(define-configuration/no-serialization forgejo-configuration
  (uid
   (positive 34595)
   "The uid assigned to the Forgejo service account.")
  (gid
   (positive 98715)
   "The gid assigned to the Forgejo service account.")
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
   (string "/var/lib/forgejo")
   "The directory where forgejo writes state.")
  (app.ini
   (maybe-file-like)
   "The @code{app.ini} configuration passed to Forgejo."))

(define (%forgejo-accounts config)
  (list (user-group
         (system? #t)
         (name "forgejo")
         (id (forgejo-configuration-gid config)))
        (user-account
         (name "forgejo")
         (uid (forgejo-configuration-uid config))
         (comment "Forgejo's Service Account")
         (group "forgejo")
         (supplementary-groups '("tty"))
         (system? #t)
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (%forgejo-activation config)
  "Return an activation gexp for Forgejo."
  (let* ((datadir (forgejo-configuration-datadir config))
         (gid
           (forgejo-configuration-gid config))
         (uid
           (forgejo-configuration-uid config)))
    #~(begin
        (use-modules (guix build utils))
        (let ((datadir #$datadir)
              (gid #$gid)
              (uid #$uid))
          ;; Setup datadir
          (mkdir-p datadir)
          ;; FIXME: Forgejo seems to ignore USER_UID and USER_GID
          (chown datadir 1000 1000)
          (chmod datadir #o770)))))

(define forgejo-configuration->oci-container-configuration
  (lambda (config)
    (let ((app.ini (forgejo-configuration-app.ini config))
          (datadir (forgejo-configuration-datadir config))
          (gid
           (forgejo-configuration-gid config))
          (image
           (forgejo-configuration-image config))
          (port
           (forgejo-configuration-port config))
          (ssh-port
           (forgejo-configuration-ssh-port config))
          (uid
           (forgejo-configuration-uid config)))
      (list (oci-container-configuration
             (image image)
             (environment
              `(("USER_UID" . ,(number->string uid))
                ("USER_GID" . ,(number->string gid))))
             (ports
              `((,port . "3000")
                (,ssh-port . "22")))
             (volumes
              `((,datadir . "/var/lib/gitea")
                ,@(if (maybe-value-set? app.ini)
                      '((,app.ini . "/etc/gitea/app.ini:ro"))
                      '())
                ("/etc/timezone" . "/etc/timezone:ro")
                ("/etc/localtime" . "/etc/localtime:ro"))))))))

(define oci-forgejo-service-type
  (service-type (name 'forgejo)
                (extensions (list (service-extension oci-container-service-type
                                                     forgejo-configuration->oci-container-configuration)
                                  (service-extension account-service-type
                                                     %forgejo-accounts)
                                  (service-extension activation-service-type
                                                     %forgejo-activation)))
                (default-value (forgejo-configuration))
                (description
                 "This service install a OCI backed Forgejo Shepherd Service.")))
