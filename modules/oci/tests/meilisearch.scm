;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Giacomo Leidi <therewasa@fishinthecalculator.me>

(define-module (oci tests meilisearch)
  #:use-module (oci services meilisearch)
  #:use-module (gnu services)
  #:use-module (gnu services containers)
  #:use-module (gnu services docker)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu tests)
  #:use-module (guix gexp)
  #:export (%oci-meilisearch-os
            %oci-meilisearch-docker-os
                                        ;%test-oci-meilisearch-docker
            %test-oci-meilisearch-rootless-podman))


;;;
;;; The SOPS service.
;;;

(define meilisearch-configuration
  (oci-meilisearch-configuration
    (log-file "/var/log/meilisearch.log")
    (network "host")
    (shepherd-requirement
     (append '(networking)
             %default-oci-meilisearch-shepherd-requirement))))

(define %oci-meilisearch-os
  (simple-operating-system
   (service dhcpcd-service-type)
   (service dbus-root-service-type)
   (service polkit-service-type)
   (service elogind-service-type)
   (service iptables-service-type)
   (service rootless-podman-service-type
            (rootless-podman-configuration
              (containers-storage
               (mixed-text-file "storage.conf"
                "[storage]
driver = \"vfs\""))))
   (service oci-service-type
            (oci-configuration
             (runtime 'podman)
             (verbose? #t)))
   (service oci-meilisearch-service-type
            meilisearch-configuration)))

;; (define %oci-meilisearch-docker-os
;;   (simple-operating-system
;;    (service dhcpcd-service-type)
;;    (service dbus-root-service-type)
;;    (service polkit-service-type)
;;    (service elogind-service-type)
;;    (service iptables-service-type)
;;    (service containerd-service-type)
;;    (service docker-service-type)
;;    (service oci-service-type
;;             (oci-configuration
;;              (verbose? #t)))
;;    (service oci-meilisearch-service-type
;;             meilisearch-configuration)))

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

                (wait-for-service 'podman-meilisearch #:timeout 80))
             marionette))

          (test-assert "meilisearch listening"
            ;; Wait until meilisearch is actually listening.
            (wait-for-tcp-port 7700 marionette #:timeout 120))

          (test-equal "meilisearch logs"
            ""
            (let ((a (marionette-eval
                      '(begin
                         (use-modules (ice-9 textual-ports))
                         (call-with-input-file "/var/log/meilisearch.log" get-string-all))
                      marionette)))
              (display a)
              #f))

          (test-end))))

  (gexp->derivation "rootless-podman-oci-meilisearch-test" test))

(define %test-oci-meilisearch-rootless-podman
  (system-test
   (name "oci-meilisearch-rootless-podman")
   (description "Test rootless podman backed Meilisearch.")
   (value (run-rootless-podman-oci-meilisearch-test))))

;; (define (run-docker-oci-meilisearch-test)
;;   (define os
;;     (marionette-operating-system
;;      (operating-system-with-gc-roots
;;       %oci-meilisearch-docker-os
;;       (list))
;;      #:imported-modules '((gnu build oci-containers)
;;                           (gnu build dbus-service)
;;                           (gnu services herd)
;;                           (guix combinators))))

;;   (define vm
;;     (virtual-machine
;;      (operating-system os)
;;      (volatile? #f)
;;      (memory-size 1024)
;;      (disk-image-size (* 5000 (expt 2 20)))
;;      (port-forwardings '())))

;;   (define test
;;     (with-imported-modules '((gnu build oci-containers)
;;                              (gnu build dbus-service)
;;                              (gnu build marionette)
;;                              (oci tests build marionette))
;;       #~(begin
;;           (use-modules (srfi srfi-1) (srfi srfi-11) (srfi srfi-64)
;;                        (gnu build dbus-service)
;;                        (gnu build marionette)
;;                        ((oci tests build marionette) #:prefix local:))

;;           (define marionette
;;             ;; Relax timeout to accommodate older systems and
;;             ;; allow for pulling the image.
;;             (local:make-marionette (list #$vm) #:timeout 60 #:user-options '("-nic" "user,model=virtio-net-pci")))

;;           (test-runner-current (system-test-runner #$output))
;;           (test-begin "docker-oci-meilisearch")

;;           (marionette-eval
;;            '(begin
;;               (use-modules (gnu services herd))
;;               (wait-for-service 'user-processes)
;;               (wait-for-service 'dockerd #:timeout 80))
;;            marionette)

;;           (test-assert "meilisearch running"
;;             (marionette-eval
;;              '(begin
;;                 (use-modules (gnu services herd))
;;                 (wait-for-service 'docker-meilisearch #:timeout 80))
;;              marionette))

;;           (test-assert "meilisearch listening"
;;             ;; Wait until meilisearch is actually listening.
;;             (wait-for-tcp-port 7700 marionette))

;;           (test-end))))

;;   (gexp->derivation "docker-oci-meilisearch-test" test))

;; (define %test-oci-meilisearch-docker
;;   (system-test
;;    (name "oci-meilisearch-docker")
;;    (description "Test docker backed Meilisearch.")
;;    (value (run-docker-oci-meilisearch-test))))
