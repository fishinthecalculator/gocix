;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Giacomo Leidi <therewasa@fishinthecalculator.me>

(define-module (oci services cap)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services containers)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (oci build utils)
  #:use-module (srfi srfi-1)
  #:export (%default-oci-cap-shepherd-requirement

            oci-cap-service-type

            oci-cap-configuration
            oci-cap-configuration?
            oci-cap-configuration-fields
            oci-cap-configuration-runtime
            oci-cap-configuration-image
            oci-cap-configuration-command
            oci-cap-configuration-port
            oci-cap-configuration-requirement
            oci-cap-configuration-data-directory
            oci-cap-configuration-log-file
            oci-cap-configuration-admin-key
            oci-cap-configuration-valkey-url
            oci-cap-configuration-network
            oci-cap-configuration-extra-variables
            oci-cap-configuration->oci-container-configuration

            oci-cap-data-directory
            oci-cap-log-file))

;; Cap is a self-hosted CAPTCHA.  Its server needs a
;; Valkey-compatible store, which the native @code{redis-service-type}
;; can satisfy when the container is attached to the host
;; network.

(define cap-tag
  "4.0.7")

(define cap-image
  (string-append "docker.io/tiago2/cap:" cap-tag))

(define (string-or-volume? value)
  (or (string? value)
      (oci-volume-configuration? value)))

(define (string-or-oci-image? value)
  (or (string? value)
      (oci-image? value)))

(define-maybe/no-serialization string)
(define-maybe/no-serialization string-or-volume)

(define %default-oci-cap-shepherd-requirement
  '(redis user-processes))

(define-configuration/no-serialization oci-cap-configuration
  (runtime
   (symbol 'docker)
   "The OCI runtime to be used for this service.")
  (image
   (string-or-oci-image cap-image)
   "The image to use for the OCI backed Shepherd service.  It can be a string,
in which case it is resolved by the OCI runtime, or an @code{oci-image} record,
which is loaded into the OCI runtime store.")
  (command
   (list-of-strings '("bun" "run" "./src/index.js"))
   "The command run inside the Cap container after exporting the configured
secrets.")
  (port
   (string "3000")
   "This host port will be mapped onto Cap's port inside the container.
When the container is attached to the @code{\"host\"} network this field is
ignored.")
  (requirement
   (list %default-oci-cap-shepherd-requirement)
   "A list of Shepherd services that will be waited for before starting Cap.
Cap needs a Valkey-compatible key/value store, which the native
@code{redis-service-type} provides.

The default list of dependencies is exposed through the symbol
@code{%default-oci-cap-shepherd-requirement}.")
  (data-directory
   (maybe-string-or-volume)
   "The directory where Cap writes state.  It can be either an
@code{oci-volume-configuration} representing the OCI volume where Cap will
write, or a string representing a file system path in the host system which
will be mapped inside the container.  By default it is
@code{\"/var/lib/cap\"}.")
  (log-file
   (maybe-string)
   "When @code{log-file} is set, it names the file to which the service's
standard output and standard error are redirected.  @code{log-file} is created
if it does not exist, otherwise it is appended to.  By default it is
@code{\"/var/log/cap.log\"}.")
  (admin-key
   (string)
   "The file name of the secret containing Cap's @code{ADMIN_KEY}, the
password for the dashboard and the admin API, at least 12 characters.  The
secret file must be readable by Cap's service user, which runs as UID 1000.")
  (valkey-url
   (maybe-string)
   "The Valkey or Redis connection string used by Cap to reach its key/value
store.  By default Cap uses @code{redis://localhost:6379}, which assumes the
container is attached to the @code{\"host\"} network and that
@code{redis-service-type} is configured.")
  (network
   (string "host")
   "The OCI network where the Cap container will be attached.  It defaults to
@code{\"host\"} so that Cap can reach the local Redis instance.  Set it to a
different network when using a remote Valkey, in which case the @code{port}
field will be mapped into the container.")
  (extra-variables
   (list '())
   "A list of pairs representing any extra environment variable that should be
set inside the container.  Refer to the
@uref{upstream, https://trycap.dev/guide/capd/options} documentation for more
details."))

(define (oci-cap-data-directory config)
  (define maybe-data-directory
    (oci-cap-configuration-data-directory config))
  (if (maybe-value-set? maybe-data-directory)
      maybe-data-directory
      "/var/lib/cap"))

(define (oci-cap-log-file config)
  (define maybe-log-file
    (oci-cap-configuration-log-file config))
  (if (maybe-value-set? maybe-log-file)
      maybe-log-file
      "/var/log/cap.log"))

(define (%cap-secrets config)
  (list (oci-cap-configuration-admin-key config)))

(define %cap-secrets-variables
  '("ADMIN_KEY"))

(define (%cap-secrets-specs config)
  (zip %cap-secrets-variables (%cap-secrets config)))

(define (cap-sh-command secrets-specs command)
  "Return a POSIX sh command that exports each of SECRETS-SPECS, a list of
(@code{variable} . @code{secret-file}) pairs, and then execs COMMAND, a list of
strings."
  (string-join
   `("set -e"
     ,@(map (lambda (spec)
              (string-append "export " (car spec)
                             "=\"$(cat " (cadr spec) ")\""))
            secrets-specs)
     ,(string-append "exec " (string-join command " ")))
   "; "))

(define (cap-accounts config)
  (if (eq? 'podman (oci-cap-configuration-runtime config))
      '()
      (list (user-account
             (name "cap")
             (comment "Cap's Service Account")
             ;; Cap runs as the "bun" user, which is UID 1000 inside the
             ;; image.  The data directory has to be owned by that UID.
             (uid 1000)
             (group "root")
             (supplementary-groups '("tty"))
             (system? #t)
             (home-directory "/var/empty")
             (shell (file-append shadow "/sbin/nologin"))))))

(define (cap-activation config)
  "Return an activation gexp for Cap."
  (let* ((runtime (oci-cap-configuration-runtime config))
         (datadir (oci-cap-data-directory config))
         (log-file (oci-cap-log-file config)))
    #~(begin
        (use-modules (guix build utils))
        (let ((log-directory (dirname #$log-file)))
          (unless (file-exists? log-directory)
            (mkdir-p log-directory)))
        #$(if (string? datadir)
              #~(let* ((user (getpwnam (if #$(eq? 'podman runtime)
                                           "oci-container" "cap")))
                       (uid (passwd:uid user))
                       (gid (passwd:gid user))
                       (datadir #$datadir))
                  (mkdir-p datadir)
                  (chown datadir uid gid)
                  (if #$(eq? 'podman runtime)
                      (chmod datadir #o700)
                      (chmod datadir #o755)))
              #~(begin)))))

(define oci-cap-configuration->oci-container-configuration
  (lambda (config)
    (let* ((datadir (oci-cap-data-directory config))
           (log-file (oci-cap-log-file config))
           (image (oci-cap-configuration-image config))
           (network (oci-cap-configuration-network config))
           (port (oci-cap-configuration-port config))
           (requirement (oci-cap-configuration-requirement config))
           (runtime (oci-cap-configuration-runtime config))
           (command (oci-cap-configuration-command config))
           (valkey-url (oci-cap-configuration-valkey-url config))
           (extra-variables (oci-cap-configuration-extra-variables config))
           (secrets (secrets-volume-mappings (%cap-secrets config)))
           (environment
            (append
             (if (maybe-value-set? valkey-url)
                 `(("VALKEY_URL" . ,valkey-url))
                 '())
             extra-variables))
           (container-config
            (oci-container-configuration
             (image image)
             (log-file log-file)
             (requirement requirement)
             (extra-arguments
              ;; When the data directory is a host directory, map Cap's
              ;; internal UID 1000 back to the user running the OCI runtime.
              (if (and (eq? runtime 'podman) (string? datadir))
                  '("--userns=keep-id:uid=1000")
                  '()))
             ;; Cap's image does not ship tini, so run the export wrapper
             ;; through sh before handing off to bun.
             (entrypoint "/bin/sh")
             (command
              `("-c"
                ,(cap-sh-command
                  (%cap-secrets-specs config)
                  command)))
             (environment environment)
             (ports
              (if (string=? network "host")
                  '()
                  `((,port . "3000"))))
             (network network)
             (volumes
              `((,(if (string? datadir)
                      datadir
                      (oci-volume-configuration-name datadir))
                 . "/usr/src/app/data")
                ,@secrets)))))
      (list container-config))))

(define oci-cap-service-type
  (service-type (name 'cap)
                (extensions
                 (list
                  (service-extension oci-service-type
                                     (lambda (config)
                                       (oci-extension
                                        (volumes
                                         (let ((datadir
                                                (oci-cap-data-directory config)))
                                           (if (oci-volume-configuration? datadir)
                                               (list datadir)
                                               '())))
                                        (containers
                                         (oci-cap-configuration->oci-container-configuration
                                          config)))))
                  (service-extension account-service-type
                                     cap-accounts)
                  (service-extension activation-service-type
                                     cap-activation)))
                (description
                 "This service install a OCI backed Cap Shepherd Service.")))
