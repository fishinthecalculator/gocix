(define-module (oci-networks-example)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services container)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (oci services grafana)
  #:use-module (oci services prometheus))

(operating-system
 ;; Dummy bootloader
 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)))
 ;; Dummy file-systems
 (file-systems %base-file-systems)
 (host-name "oci-networks-example-host")
 (packages %base-packages)
 (services
  (append
   (list
    (service dhcpcd-client-service-type)
    (service elogind-service-type)
    (service dbus-root-service-type)
    (service containerd-service-type)
    (service docker-service-type)
    (simple-service 'oci-networks
                    oci-service-type
                    (oci-extension
                     (networks
                      (list
                       (oci-network-configuration
                        (name "monitoring"))))))

    (service oci-prometheus-service-type
             (oci-prometheus-configuration
              (network "monitoring")
              (record
               (prometheus-configuration
                (global
                 (prometheus-global-configuration
                  (scrape-interval "30s")
                  (scrape-timeout "12s")))
                (scrape-configs
                 (list
                  (prometheus-scrape-configuration
                   (job-name "prometheus")
                   (static-configs
                    (list (prometheus-static-configuration
                           (targets '("docker-prometheus:9090"))))))))))))

    (service oci-grafana-service-type
             (oci-grafana-configuration
              (network "monitoring"))))
   %base-services)))
