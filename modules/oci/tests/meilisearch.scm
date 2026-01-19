;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Giacomo Leidi <therewasa@fishinthecalculator.me>

(define-module (oci tests meilisearch)
  #:use-module (oci services meilisearch)
  #:use-module (gnu services)
  #:use-module (gnu services containers)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu tests)
  #:use-module (guix gexp)
  #:export (%test-oci-meilisearch-rootless-podman))


;;;
;;; The SOPS service.
;;;

(define %oci-meilisearch-os
  (simple-operating-system
   (service dhcpcd-service-type)
   (service dbus-root-service-type)
   (service polkit-service-type)
   (service elogind-service-type)
   (service iptables-service-type)
   (service rootless-podman-service-type)
   (service oci-service-type
            (oci-configuration
             (runtime 'podman)
             (verbose? #t)))
   (service oci-meilisearch-service-type
         (oci-meilisearch-configuration
          (network "host")))))

(define (run-rootless-podman-oci-meilisearch-test)
  (define os
    (marionette-operating-system
     (operating-system-with-gc-roots
      %oci-meilisearch-os
      (list))
     #:imported-modules '((gnu build oci-containers)
                          (gnu build dbus-service)
                          (gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (volatile? #f)
     (memory-size 1024)
     (disk-image-size (* 5000 (expt 2 20)))
     (port-forwardings '())))

  (define test
    (with-imported-modules '((gnu build oci-containers)
                             (gnu build dbus-service)
                             (gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-1) (srfi srfi-11) (srfi srfi-64)
                       (gnu build dbus-service)
                       (gnu build marionette))

          (define marionette
            ;; Relax timeout to accommodate older systems and
            ;; allow for pulling the image.
            (make-marionette (list #$vm) #:timeout 60))

          (test-runner-current (system-test-runner #$output))
          (test-begin "rootless-podman-oci-meilisearch")

          (marionette-eval
           '(begin
              (use-modules (gnu services herd))
              (wait-for-service 'user-processes))
           marionette)

          (test-assert "meilisearch running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (wait-for-service 'podman-meilisearch))
             marionette))

          ;; (test-assert "podman-volumes running"
          ;;   (begin
          ;;     (define (run-test)
          ;;       (first
          ;;        (marionette-eval
          ;;         `(begin
          ;;            #$@(%oci-rootless-podman-run
          ;;                #~((oci-object-service-available?
          ;;                    "/run/current-system/profile/bin/podman"
          ;;                    "volume"
          ;;                    '("my-volume")
          ;;                    #:verbose? #t))))
          ;;         marionette)))
          ;;     ;; Allow services to come up on slower machines.
          ;;     (with-retries 80 1 (equal? '("my-volume") (run-test)))))

          (test-end))))

  (gexp->derivation "rootless-podman-oci-meilisearch-test" test))

(define %test-oci-meilisearch-rootless-podman
  (system-test
   (name "oci-meilisearch-rootless-podman")
   (description "Test rootless podman backed Meilisearch.")
   (value (run-rootless-podman-oci-meilisearch-test))))
