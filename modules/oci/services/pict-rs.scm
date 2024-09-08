;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (oci services pict-rs)
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
            oci-pict-rs-configuration-image
            oci-pict-rs-configuration-port
            oci-pict-rs-configuration-datadir
            oci-pict-rs-configuration-log-file
            oci-pict-rs-configuration-network
            oci-pict-rs-configuration-config-file
            oci-pict-rs-configuration->oci-container-configuration
            oci-pict-rs-service-type))

(define %pict-rs-default.toml
  (plain-file
   "default.toml"
   "# This content comes from https://git.asonix.dog/asonix/pict-rs/src/branch/main/defaults.toml
[server]
address = \"0.0.0.0:8080\"
read_only = false
danger_dummy_mode = false
max_file_count = 1
temporary_directory = \"/tmp\"
cleanup_temporary_directory = true

[client]
timeout = 30

[upgrade]
concurrency = 32

[tracing.logging]
format = \"normal\"
targets = \"info\"
log_spans = false
no_ansi = false
log_requests = false

[tracing.console]
buffer_capacity = 102400

[tracing.opentelemetry]
service_name = \"pict-rs\"
targets = \"info\"

[metrics]

[old_repo]

[media]
external_validation_timeout = 30
max_file_size = 40
process_timeout = 30
filters = [
    \"blur\",
    \"crop\",
    \"identity\",
    \"resize\",
    \"thumbnail\",
]

[media.retention]
variants = \"7d\"
proxy = \"7d\"

[media.magick]
max_width = 10000
max_height = 10000
max_area = 20000
memory = 256
map = 512
disk = 1024

[media.image]
max_width = 10000
max_height = 10000
max_area = 40000000
max_file_size = 40

[media.animation]
max_width = 1920
max_height = 1920
max_area = 2073600
max_file_size = 40
max_frame_count = 900

[media.video]
enable = true
allow_audio = false
max_width = 3840
max_height = 3840
max_area = 8294400
max_file_size = 40
max_frame_count = 900

[media.video.quality]
crf_max = 32

[repo]
type = \"sled\"
path = \"/mnt/sled-repo\"
cache_capacity = 67108864
export_path = \"/mnt/exports\"

[store]
type = \"filesystem\"
path = \"/mnt/files\"
"))

(define pict-rs-tag
  "0.5")

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
  (log-file
   (string "/var/log/pict-rs.log")
   "The path where pict-rs writes logs.")
  (network
   (maybe-string)
   "The docker network where the pict-rs container will be attached. When equal
to \"host\" the @code{port} field will not be mapped into the container's one.")
  (config-file
   (file-like %pict-rs-default.toml)
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
             (config-file
              (oci-pict-rs-configuration-config-file config))
             (network
              (oci-pict-rs-configuration-network config))
             (log-file
              (oci-pict-rs-configuration-log-file config))
             (image
              (oci-pict-rs-configuration-image config))
             (port
              (oci-pict-rs-configuration-port config))
             (container-config
              (oci-container-configuration
               (image image)
               (log-file log-file)
               (entrypoint
                "/sbin/tini")
               (command
                '("/usr/local/bin/pict-rs" "--config-file" "/pict-rs.toml" "run"))
               (ports
                `((,port . ,port)))
               (volumes
                `((,config-file . "/pict-rs.toml:ro")
                  ("/gnu/store" . "/gnu/store")
                  (,datadir . "/mnt"))))))
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
                (default-value (oci-pict-rs-configuration))
                (description
                 "This service install a OCI backed pict-rs Shepherd Service.")))
