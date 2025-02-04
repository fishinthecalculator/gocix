;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (oci home services containers)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix packages)
  #:use-module (oci services containers)
  #:use-module (srfi srfi-1)
  #:export (home-oci-configuration
            home-oci-configuration?
            home-oci-configuration-fields
            home-oci-configuration-runtime
            home-oci-configuration-runtime-cli
            home-oci-configuration-containers
            home-oci-configuration-networks
            home-oci-configuration-volumes
            home-oci-configuration-verbose?

            home-oci-extension
            home-oci-extension?
            home-oci-extension-fields
            home-oci-extension-containers
            home-oci-extension-networks
            home-oci-extension-volumes

            home-oci-service-type
            home-oci-configuration->shepherd-services))

(define (package-or-string? value)
  (or (package? value) (string? value)))

(define-maybe/no-serialization package-or-string)

(define-configuration/no-serialization home-oci-configuration
  (runtime
   (symbol 'docker)
   "The OCI runtime to use to run commands.  It can be either @code{'docker} or
@code{'podman}."
   (sanitizer oci-sanitize-runtime))
  (runtime-cli
   (maybe-package-or-string)
   "The OCI runtime command line to be installed in the system profile and used
to provision OCI resources.  When unset it will default to @code{docker-cli}
package for the @code{'docker} runtime or to @code{podman} package for the
@code{'podman} runtime.  When a string is passed it will be interpreted as the
absolute file-system path of the selected OCI runtime command.")
  (containers
   (list-of-oci-containers '())
   "The list of @code{oci-container-configuration} records representing the
containers to provision.  Most users are supposed not to use this field and use
the @code{oci-extension} record instead.")
  (networks
   (list-of-oci-networks '())
   "The list of @code{oci-network-configuration} records representing the
networks to provision.  Most users are supposed not to use this field and use
the @code{oci-extension} record instead.")
  (volumes
   (list-of-oci-volumes '())
   "The list of @code{oci-volume-configuration} records representing the
volumes to provision.  Most users are supposed not to use this field and use
the @code{oci-extension} record instead.")
  (verbose?
   (boolean #f)
   "When true, additional output will be printed, allowing to better follow the
flow of execution."))

(define-configuration/no-serialization home-oci-extension
  (containers
   (list-of-oci-containers '())
   "The list of @code{oci-container-configuration} records representing the
containers to add.")
  (networks
   (list-of-oci-networks '())
   "The list of @code{oci-network-configuration} records representing the
networks to add.")
  (volumes
   (list-of-oci-volumes '())
   "The list of @code{oci-volume-configuration} records representing the
volumes to add."))

(define* (oci-runtime-home-cli config)
  (let ((runtime-cli
         (home-oci-configuration-runtime-cli config))
        (runtime
         (home-oci-configuration-runtime config)))
    (oci-runtime-cli runtime runtime-cli
                     (string-append (getenv "HOME")
                                    "/.guix-home/profile"))))

(define (home-oci-networks-shepherd-name runtime)
  (string-append "home-" (oci-networks-shepherd-name runtime)))

(define (home-oci-volumes-shepherd-name runtime)
  (string-append "home-" (oci-volumes-shepherd-name runtime)))

(define (home-oci-configuration->shepherd-services config)
  (let ((runtime (home-oci-configuration-runtime config))
        (runtime-cli
         (oci-runtime-home-cli config))
        (containers (home-oci-configuration-containers config))
        (networks (home-oci-configuration-networks config))
        (volumes (home-oci-configuration-volumes config))
        (verbose? (home-oci-configuration-verbose? config)))
    (oci-configuration->shepherd-services runtime runtime-cli containers networks volumes
                                          #:verbose? verbose?
                                          #:networks-name
                                          (home-oci-networks-shepherd-name runtime)
                                          #:volumes-name
                                          (home-oci-volumes-shepherd-name runtime))))

(define (home-oci-extension-merge a b)
  (home-oci-extension
   (containers (oci-objects-merge-lst
                (home-oci-extension-containers a)
                (home-oci-extension-containers b)
                "container"
                (lambda (config)
                  (define maybe-name (oci-container-configuration-provision config))
                  (if (maybe-value-set? maybe-name)
                      maybe-name
                      (oci-image->container-name
                       (oci-container-configuration-image config))))))
   (networks (oci-objects-merge-lst
              (home-oci-extension-networks a)
              (home-oci-extension-networks b)
              "network"
              (lambda (runtime)
                (string-append "" (oci-networks-shepherd-name runtime)))))
   (volumes (oci-objects-merge-lst
             (home-oci-extension-volumes a)
             (home-oci-extension-volumes b)
             "volume"
             oci-volumes-shepherd-name))))

(define home-oci-service-type
  (service-type (name 'home-oci)
                (extensions
                 (list
                  (service-extension home-profile-service-type
                                     (lambda (config)
                                       (let ((runtime-cli
                                              (home-oci-configuration-runtime-cli config))
                                             (runtime
                                              (home-oci-configuration-runtime config)))
                                         (oci-service-profile runtime runtime-cli))))
                  (service-extension home-shepherd-service-type
                                     home-oci-configuration->shepherd-services)))
                ;; Concatenate OCI object lists.
                (compose (lambda (args)
                           (fold home-oci-extension-merge
                                 (home-oci-extension)
                                 args)))
                (extend
                 (lambda (config extension)
                   (home-oci-configuration
                    (inherit config)
                    (containers
                     (oci-objects-merge-lst
                      (oci-configuration-containers config)
                      (oci-extension-containers extension)
                      "container"
                      (lambda (oci-config)
                        (define runtime
                          (oci-configuration-runtime config))
                        (oci-container-shepherd-name runtime oci-config))))
                    (networks (oci-objects-merge-lst
                               (oci-configuration-networks config)
                               (oci-extension-networks extension)
                               "network"
                               home-oci-networks-shepherd-name))
                    (volumes (oci-objects-merge-lst
                              (oci-configuration-volumes config)
                              (oci-extension-volumes extension)
                              "volume"
                              home-oci-volumes-shepherd-name)))))
                (default-value (home-oci-configuration))
                (description
                 "This service implements the provisioning of OCI object such
as containers, networks and volumes.")))
