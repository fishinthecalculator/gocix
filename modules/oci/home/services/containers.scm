;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (oci home services containers)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (oci services containers)
  #:use-module (srfi srfi-1)
  #:export (home-oci-service-type))

(define home-oci-service-type
  (service-type (inherit (system->home-service-type oci-service-type))
                (extensions
                 (list
                  (service-extension home-profile-service-type
                                     (oci-service-extension-wrap-validate
                                      (lambda (config)
                                        (let ((runtime-cli
                                               (oci-configuration-runtime-cli config))
                                              (runtime
                                               (oci-configuration-runtime config)))
                                          (oci-service-profile runtime runtime-cli)))))
                  (service-extension home-shepherd-service-type
                                     (oci-service-extension-wrap-validate
                                      oci-configuration->shepherd-services))))
                (extend
                 (lambda (config extension)
                   (for-home
                    (oci-configuration
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
                                oci-network-configuration-name))
                     (volumes (oci-objects-merge-lst
                               (oci-configuration-volumes config)
                               (oci-extension-volumes extension)
                               "volume"
                               oci-volume-configuration-name))))))
                (default-value (for-home (oci-configuration)))))
