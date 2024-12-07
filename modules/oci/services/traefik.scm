;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (oci services traefik)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services docker)
  #:use-module (gnu system shadow)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module ((oci services containers) #:prefix oci:)
  #:use-module (sops secrets)
  #:use-module (sops services sops)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (oci-whoami-configuration
            oci-whoami-configuration?
            oci-whoami-configuration-fields
            oci-whoami-configuration-image
            oci-whoami-configuration-port
            oci-whoami-configuration-requirement
            oci-whoami-configuration-log-file
            oci-whoami-configuration-cert
            oci-whoami-configuration-key
            oci-whoami-configuration-network
            oci-whoami-configuration-name
            oci-whoami-configuration-oci-extra-arguments
            oci-whoami-configuration->oci-container-configuration
            oci-whoami-service-type))

(define whoami-tag
  "v1.10.3")

(define whoami-image
  (string-append "docker.io/traefik/whoami:" whoami-tag))

(define-maybe string)

(define-configuration/no-serialization oci-whoami-configuration
  (image
   (string whoami-image)
   "The image to use for the OCI backed Shepherd service.")
  (port
   (string "8080")
   "The port where whoami will be exposed.")
  (requirement
   (list '())
   "A list of Shepherd services that will be waited for before starting @code{whoami}.")
  (log-file
   (string "/var/log/whoami.log")
   "The path where whoami writes logs.")
  (cert
   (maybe-string)
   "The path to a SSL certificate.")
  (key
   (maybe-string)
   "The path to a SSL key.")
  (network
   (maybe-string)
   "The docker network where the whoami container will be attached. When equal
to \"host\" the @code{port} field will not be mapped into the container's one.")
  (name
   (maybe-string)
   "The name for the @code{whoami} process.")
  (oci-extra-arguments
   (list '())
   "A list of strings, gexps or file-like objects that will be directly passed
to the OCI runtime invokation.  You can use this field to set labels for example."))

(define (%whoami-activation config)
  "Return an activation gexp for whoami."
  (let ((log-file (oci-whoami-configuration-log-file config)))
    #~(begin
        ;; Setup log-dir
        (let ((logs-directory (dirname #$log-file)))
          (unless (file-exists? logs-directory)
            (mkdir-p logs-directory))))))

(define (oci-whoami-configuration->options config)
  (let ((cert (oci-whoami-configuration-cert config))
        (key
         (oci-whoami-configuration-key config))
        (network
         (oci-whoami-configuration-network config))
        (name
         (oci-whoami-configuration-name config))
        (port
         (oci-whoami-configuration-port config)))
    (apply append
           (filter (compose not unspecified?)
                   `(,(if (maybe-value-set? cert)
                          `("--cert" ,cert)
                          '())
                     ,(if (maybe-value-set? key)
                          `("--key")
                          '())
                     ,(if (maybe-value-set? name)
                          `("--name" ,name)
                          '())
                     ,(if (and
                           (maybe-value-set? network)
                           (string=? network "host"))
                          `("--port" ,port)
                          '()))))))

(define oci-whoami-configuration->oci-container-configuration
  (lambda (config)
    (let* ((requirement
            (oci-whoami-configuration-requirement config))
           (cert (oci-whoami-configuration-cert config))
           (extra-arguments
            (oci-whoami-configuration-oci-extra-arguments config))
           (key
            (oci-whoami-configuration-key config))
           (network
            (oci-whoami-configuration-network config))
           (log-file
            (oci-whoami-configuration-log-file config))
           (image
            (oci-whoami-configuration-image config))
           (port
            (oci-whoami-configuration-port config))
           (command
            (oci-whoami-configuration->options config))
           (container-config
            (oci-container-configuration
             (image image)
             (log-file log-file)
             (requirement requirement)
             (command command)
             (ports
              `((,port . "80")))
             (volumes
              `(,@(if (maybe-value-set? cert)
                      `((,cert . ,(string-append cert ":ro")))
                      '())
                ,@(if (maybe-value-set? key)
                      `((,key . ,(string-append key ":ro")))
                      '())))
             (extra-arguments extra-arguments))))

      (list
       (if (maybe-value-set? network)
           (oci-container-configuration
            (inherit container-config)
            (ports '())
            (network network))
           container-config)))))

(define oci-whoami-service-type
  (service-type (name 'whoami)
                (extensions (list (service-extension oci:oci-container-service-type
                                                     oci-whoami-configuration->oci-container-configuration)
                                  (service-extension activation-service-type
                                                     %whoami-activation)))
                (default-value (oci-whoami-configuration))
                (description
                 "This service install a OCI backed whoami Shepherd Service.")))
