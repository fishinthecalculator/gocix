;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024, 2025 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (oci services containers)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages containers)
  #:use-module (gnu packages docker)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module ((gnu services containers) #:prefix mainline:)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (oci self)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)

  #:export (oci-container-configuration->options
            oci-container-service-type

            list-of-oci-containers?
            list-of-oci-networks?
            list-of-oci-volumes?

            %oci-supported-runtimes
            oci-runtime-system-environment
            oci-runtime-system-extra-arguments
            oci-runtime-system-group
            oci-runtime-system-requirement
            oci-runtime-cli
            oci-runtime-system-cli
            oci-runtime-name
            oci-runtime-group

            oci-network-configuration
            oci-network-configuration?
            oci-network-configuration-fields
            oci-network-configuration-name
            oci-network-configuration-driver
            oci-network-configuration-gateway
            oci-network-configuration-internal?
            oci-network-configuration-ip-range
            oci-network-configuration-ipam-driver
            oci-network-configuration-ipv6?
            oci-network-configuration-subnet
            oci-network-configuration-labels
            oci-network-configuration-extra-arguments

            oci-volume-configuration
            oci-volume-configuration?
            oci-volume-configuration-fields
            oci-volume-configuration-name
            oci-volume-configuration-labels
            oci-volume-configuration-extra-arguments

            oci-configuration
            oci-configuration?
            oci-configuration-fields
            oci-configuration-runtime
            oci-configuration-runtime-cli
            oci-configuration-runtime-extra-arguments
            oci-configuration-user
            oci-configuration-group
            oci-configuration-containers
            oci-configuration-networks
            oci-configuration-volumes
            oci-configuration-verbose?
            oci-configuration-valid?

            oci-extension
            oci-extension?
            oci-extension-fields
            oci-extension-containers
            oci-extension-networks
            oci-extension-volumes

            oci-container-shepherd-name
            oci-networks-shepherd-name
            oci-volumes-shepherd-name

            oci-container-shepherd-service
            oci-objects-merge-lst
            oci-extension-merge
            oci-service-extension-wrap-validate
            oci-service-type
            oci-service-accounts
            oci-service-profile
            oci-service-subids
            oci-state->shepherd-services
            oci-configuration->shepherd-services
            oci-configuration-extend))

;;;
;;; This is the development implementation of the Guix OCI container system service.
;;;
;;; You can find the mainline implementation at
;;; https://git.guix.gnu.org/guix.git/src/branch/master/gnu/services/containers.scm
;;;


;;;
;;; OCI provisioning service.
;;;

(define %oci-supported-runtimes
  '(docker podman))

(define (oci-runtime-system-requirement runtime)
  "Return a list of Shepherd service names required by a given OCI runtime,
before it is able to run containers."
  (if (eq? 'podman runtime)
      '(cgroups2-fs-owner cgroups2-limits
        rootless-podman-shared-root-fs user-processes)
      '(dockerd user-processes)))

(define (oci-runtime-name runtime)
  "Return a human readable name for a given OCI runtime."
  (if (eq? 'podman runtime)
      "Podman" "Docker"))

(define (oci-runtime-group runtime maybe-group)
  "Implement the logic behind selection of the group that is to be used by
Shepherd to execute OCI commands."
  (if (maybe-value-set? maybe-group)
      maybe-group
      (if (eq? 'podman runtime)
          "cgroup"
          "docker")))

(define (oci-runtime? value)
  (unless (member value %oci-supported-runtimes)
    (raise
     (formatted-message
      (G_ "OCI runtime must be a symbol and one of ~a,
but ~a was found") %oci-supported-runtimes value)))
  (symbol? value))

(define (oci-sanitize-pair pair delimiter)
  (define (valid? member)
    (or (string? member)
        (gexp? member)
        (file-like? member)))
  (match pair
    (((? valid? key) . (? valid? value))
     #~(string-append #$key #$delimiter #$value))
    (_
     (raise
      (formatted-message
       (G_ "pair members must contain only strings, gexps or file-like objects
but ~a was found")
       pair)))))

(define (oci-sanitize-mixed-list name value delimiter)
  (map
   (lambda (el)
     (cond ((string? el) el)
           ((pair? el) (oci-sanitize-pair el delimiter))
           (else
            (raise
             (formatted-message
              (G_ "~a members must be either a string or a pair but ~a was
found!")
              name el)))))
   value))

(define (oci-sanitize-host-environment value)
  ;; Expected spec format:
  ;; '(("HOME" . "/home/nobody") "JAVA_HOME=/java")
  (oci-sanitize-mixed-list "host-environment" value "="))

(define (oci-container-host-environment? value)
  (list? (oci-sanitize-host-environment value)))

(define (oci-sanitize-environment value)
  ;; Expected spec format:
  ;; '(("HOME" . "/home/nobody") "JAVA_HOME=/java")
  (oci-sanitize-mixed-list "environment" value "="))

(define (oci-container-environment? value)
  (list? (oci-sanitize-environment value)))

(define (oci-sanitize-ports value)
  ;; Expected spec format:
  ;; '(("8088" . "80") "2022:22")
  (oci-sanitize-mixed-list "ports" value ":"))

(define (oci-container-ports? value)
  (list? (oci-sanitize-ports value)))

(define (oci-sanitize-volumes value)
  ;; Expected spec format:
  ;; '(("/mnt/dir" . "/dir") "/run/current-system/profile:/java")
  (oci-sanitize-mixed-list "volumes" value ":"))

(define (oci-container-volumes? value)
  (list? (oci-sanitize-volumes value)))

(define (oci-sanitize-labels value)
  ;; Expected spec format:
  ;; '(("foo" . "bar") "foo=bar")
  (oci-sanitize-mixed-list "labels" value "="))

(define (oci-object-labels? value)
  (list? (oci-sanitize-labels value)))

(define (oci-sanitize-shepherd-actions value)
  (map
   (lambda (el)
     (if (shepherd-action? el)
         el
         (raise
          (formatted-message
           (G_ "shepherd-actions may only be shepherd-action records
but ~a was found") el))))
   value))

(define (oci-container-shepherd-actions? value)
  (list? (oci-sanitize-shepherd-actions value)))

(define (oci-sanitize-extra-arguments value)
  (define (valid? member)
    (or (string? member)
        (gexp? member)
        (file-like? member)))
  (map
   (lambda (el)
     (if (valid? el)
         el
         (raise
          (formatted-message
           (G_ "extra arguments may only be strings, gexps or file-like objects
but ~a was found") el))))
   value))

(define (oci-object-extra-arguments? value)
  (list? (oci-sanitize-extra-arguments value)))

(define (oci-image-reference image)
  "Return a string OCI image reference representing IMAGE."
  (define reference
    (if (string? image)
        image
        (string-append (mainline:oci-image-repository image)
                       ":" (mainline:oci-image-tag image))))
  (if (> (length (string-split reference #\/)) 1)
        reference
        (string-append "localhost/" reference)))

(define (oci-lowerable-image? image)
  (or (manifest? image)
      (operating-system? image)
      (gexp? image)
      (file-like? image)))

(define (string-or-oci-image? image)
  (or (string? image)
      (oci-image? image)))

(define list-of-symbols?
  (list-of symbol?))

(define (list-of-oci-records? name predicate value)
  (map
   (lambda (el)
     (if (predicate el)
         el
         (raise
          (formatted-message
           (G_ "~a contains an illegal value: ~a") name el))))
   value))

(define (list-of-oci-containers? value)
  (list-of-oci-records? "containers" mainline:oci-container-configuration? value))

(define-maybe/no-serialization string)
(define-maybe/no-serialization package)
(define-maybe/no-serialization subid-range)

(define-configuration/no-serialization oci-volume-configuration
  (name
   (string)
   "The name of the OCI volume to provision.")
  (labels
   (oci-object-labels '())
   "The list of labels that will be used to tag the current volume."
   (sanitizer oci-sanitize-labels))
  (extra-arguments
   (oci-object-extra-arguments '())
   "A list of strings, gexps or file-like objects that will be directly passed
to the @command{docker volume create} or @command{podman volume create}
invocation."
   (sanitizer oci-sanitize-extra-arguments)))

(define (list-of-oci-volumes? value)
  (list-of-oci-records? "volumes" oci-volume-configuration? value))

(define-configuration/no-serialization oci-network-configuration
  (name
   (string)
   "The name of the OCI network to provision.")
  (driver
   (maybe-string)
   "The driver to manage the network.")
  (gateway
   (maybe-string)
   "IPv4 or IPv6 gateway for the subnet.")
  (internal?
   (boolean #f)
   "Restrict external access to the network")
  (ip-range
   (maybe-string)
   "Allocate container ip from a sub-range in CIDR format.")
  (ipam-driver
   (maybe-string)
   "IP Address Management Driver.")
  (ipv6?
   (boolean #f)
   "Enable IPv6 networking.")
  (subnet
   (maybe-string)
   "Subnet in CIDR format that represents a network segment.")
  (labels
   (oci-object-labels '())
   "The list of labels that will be used to tag the current volume."
   (sanitizer oci-sanitize-labels))
  (extra-arguments
   (oci-object-extra-arguments '())
   "A list of strings, gexps or file-like objects that will be directly passed
to the @command{docker network create} or @command{podman network create}
invocation."
   (sanitizer oci-sanitize-extra-arguments)))

(define (list-of-oci-networks? value)
  (list-of-oci-records? "networks" oci-network-configuration? value))

(define (package-or-string? value)
  (or (package? value) (string? value)))

(define-maybe/no-serialization package-or-string)

(define-configuration/no-serialization oci-configuration
  (runtime
   (oci-runtime 'docker)
   "The OCI runtime to use to run commands.  It can be either @code{'docker} or
@code{'podman}.")
  (runtime-cli
   (maybe-package-or-string)
   "The OCI runtime command line to be installed in the system profile and used
to provision OCI resources, it can be either a package or a string representing
an absolute file name to the runtime binary entrypoint.  When unset it will default
to @code{docker-cli} package for the @code{'docker} runtime or to @code{podman}
package for the @code{'podman} runtime.")
  (runtime-extra-arguments
   (list '())
   "A list of strings, gexps or file-like objects that will be placed
after each @command{docker} or @command{podman} invokation.")
  (user
   (string "oci-container")
   "The user name under whose authority OCI runtime commands will be run.")
  (group
   (maybe-string)
   "The group name under whose authority OCI commands will be run.  When
using the @code{'podman} OCI runtime, this field will be ignored and the
default group of the user configured in the @code{user} field will be used.")
  (subuids-range
   (maybe-subid-range)
   "An optional @code{subid-range} record allocating subuids for the user from
the @code{user} field.  When unset, with the rootless Podman OCI runtime, it
defaults to @code{(subid-range (name \"oci-container\"))}.")
  (subgids-range
   (maybe-subid-range)
   "An optional @code{subid-range} record allocating subgids for the user from
the @code{user} field.  When unset, with the rootless Podman OCI runtime, it
defaults to @code{(subid-range (name \"oci-container\"))}.")
  (containers
   (list-of-oci-containers '())
   "The list of @code{oci-container-configuration} records representing the
containers to provision.  The use of the @code{oci-extension} record should
be preferred for most cases.")
  (networks
   (list-of-oci-networks '())
   "The list of @code{oci-network-configuration} records representing the
networks to provision.  The use of the @code{oci-extension} record should
be preferred for most cases.")
  (volumes
   (list-of-oci-volumes '())
   "The list of @code{oci-volume-configuration} records representing the
volumes to provision.  The use of the @code{oci-extension} record should
be preferred for most cases.")
  (verbose?
   (boolean #f)
   "When true, additional output will be printed, allowing to better follow the
flow of execution.")
  (home-service?
   (boolean for-home?)
   "This is an internal field denoting whether this configuration is used in a
Guix Home context, as opposed to the default Guix System context."))

(define (oci-runtime-system-environment runtime user)
  (if (eq? runtime 'podman)
      (list
       #~(string-append
          "HOME=" (passwd:dir (getpwnam #$user))))
      #~()))

(define (oci-runtime-cli runtime runtime-cli profile-directory)
  "Return a gexp that, when lowered, evaluates to the  of the OCI
runtime command requested by the user."
  (if (string? runtime-cli)
      ;; It is a user defined absolute file name.
      runtime-cli
      #~(string-append
         #$(if (maybe-value-set? runtime-cli)
               runtime-cli
               profile-directory)
         #$(if (eq? 'podman runtime)
               "/bin/podman"
               "/bin/docker"))))

(define* (oci-runtime-system-cli config #:key (profile-directory "/run/current-system/profile"))
  (let ((runtime-cli
         (oci-configuration-runtime-cli config))
        (runtime
         (oci-configuration-runtime config)))
    (oci-runtime-cli runtime runtime-cli profile-directory)))

(define (oci-runtime-home-cli config)
  (let ((runtime-cli
         (oci-configuration-runtime-cli config))
        (runtime
         (oci-configuration-runtime config)))
    (oci-runtime-cli runtime runtime-cli
                     (string-append (getenv "HOME")
                                    "/.guix-home/profile"))))

(define-configuration/no-serialization oci-extension
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

(define (oci-image->container-name image)
  "Infer the name of an OCI backed Shepherd service from its OCI image."
  (basename
   (if (string? image)
       (first (string-split image #\:))
       (mainline:oci-image-repository image))))

(define (oci-entrypoint-shepherd-action object-name entrypoint)
  "Return a Shepherd action printing a given ENTRYPOINT of an OCI object
for the given OBJECT-NAME."
  (shepherd-action
   (name 'entrypoint)
   (documentation
    (format #f "Prints ~a's OCI runtime entrypoint."
            object-name))
   (procedure
    #~(lambda _
        (format #t "~a~%" #$entrypoint)))))

(define (oci-command-line-shepherd-action object-name invocation)
  "Return a Shepherd action printing a given INVOCATION of an OCI command for the
given OBJECT-NAME."
  (shepherd-action
   (name 'command-line)
   (documentation
    (format #f "Prints ~a's OCI runtime command line invocation."
            object-name))
   (procedure
    #~(lambda _
        (format #t "~a~%" #$invocation)))))

(define (oci-container-shepherd-name runtime config)
  "Return the name of an OCI backed Shepherd service based on CONFIG.
The name configured in the configuration record is returned when
CONFIG's name field has a value, otherwise a name is inferred from CONFIG's
image field."
  (define name (mainline:oci-container-configuration-provision config))
  (define image (mainline:oci-container-configuration-image config))

  (if (maybe-value-set? name)
      name
      (string-append (symbol->string runtime) "-"
                     (oci-image->container-name image))))

(define (oci-networks-shepherd-name runtime)
  "Return the name of the OCI networks provisioning Shepherd service based on
RUNTIME."
  (string-append (symbol->string runtime) "-networks"))

(define (oci-volumes-shepherd-name runtime)
  "Return the name of the OCI volumes provisioning Shepherd service based on
RUNTIME."
  (string-append (symbol->string runtime) "-volumes"))

(define (oci-networks-home-shepherd-name runtime)
  "Return the name of the OCI volumes provisioning Home Shepherd service based on
RUNTIME."
  (string-append "home-" (oci-networks-shepherd-name runtime)))

(define (oci-volumes-home-shepherd-name runtime)
  "Return the name of the OCI volumes provisioning Home Shepherd service based on
RUNTIME."
  (string-append "home-" (oci-volumes-shepherd-name runtime)))

(define (oci-container-configuration->options config)
  "Map CONFIG, an oci-container-configuration record, to a gexp that, upon
lowering, will be evaluated to a list of strings containing command line options
for the OCI runtime run command."
  (let ((entrypoint
         (mainline:oci-container-configuration-entrypoint config))
        (network
         (mainline:oci-container-configuration-network config))
        (user
         (mainline:oci-container-configuration-container-user config))
        (workdir
         (mainline:oci-container-configuration-workdir config)))
    (apply append
           (filter (compose not unspecified?)
                   `(,(if (maybe-value-set? entrypoint)
                          `("--entrypoint" ,entrypoint)
                          '())
                     ,(append-map
                       (lambda (spec)
                         (list "--env" spec))
                       (mainline:oci-container-configuration-environment config))
                     ,(if (maybe-value-set? network)
                          `("--network" ,network)
                          '())
                     ,(if (maybe-value-set? user)
                          `("--user" ,user)
                          '())
                     ,(if (maybe-value-set? workdir)
                          `("--workdir" ,workdir)
                          '())
                     ,(append-map
                       (lambda (spec)
                         (list "-p" spec))
                       (mainline:oci-container-configuration-ports config))
                     ,(append-map
                       (lambda (spec)
                         (list "-v" spec))
                       (mainline:oci-container-configuration-volumes config)))))))

(define (oci-network-configuration->options config)
  "Map CONFIG, an oci-network-configuration record, to a gexp that, upon
lowering, will be evaluated to a list of strings containing command line options
for the OCI runtime network create command."
  (let ((driver (oci-network-configuration-driver config))
        (gateway
         (oci-network-configuration-gateway config))
        (internal?
         (oci-network-configuration-internal? config))
        (ip-range
         (oci-network-configuration-ip-range config))
        (ipam-driver
         (oci-network-configuration-ipam-driver config))
        (ipv6?
         (oci-network-configuration-ipv6? config))
        (subnet
         (oci-network-configuration-subnet config)))
    (apply append
           (filter (compose not unspecified?)
                   `(,(if (maybe-value-set? driver)
                          `("--driver" ,driver)
                          '())
                     ,(if (maybe-value-set? gateway)
                          `("--gateway" ,gateway)
                          '())
                     ,(if internal?
                          `("--internal")
                          '())
                     ,(if (maybe-value-set? ip-range)
                          `("--ip-range" ,ip-range)
                          '())
                     ,(if (maybe-value-set? ipam-driver)
                          `("--ipam-driver" ,ipam-driver)
                          '())
                     ,(if ipv6?
                          `("--ipv6")
                          '())
                     ,(if (maybe-value-set? subnet)
                          `("--subnet" ,subnet)
                          '())
                     ,(append-map
                       (lambda (spec)
                         (list "--label" spec))
                       (oci-network-configuration-labels config)))))))

(define (oci-volume-configuration->options config)
  "Map CONFIG, an oci-volume-configuration record, to a gexp that, upon
lowering, will be evaluated to a list of strings containing command line options
for the OCI runtime volume create command."
  (append-map
   (lambda (spec)
     (list "--label" spec))
   (oci-volume-configuration-labels config)))

(define lower-oci-image
  (@@ (gnu services containers) lower-oci-image))

(define-record-type* <oci-runtime-state>
  oci-runtime-state
  make-oci-runtime-state
  oci-runtime-state?
  this-oci-runtime-state

  (runtime                  oci-runtime-state-runtime
                            (default 'docker))
  (runtime-cli              oci-runtime-state-runtime-cli)
  (user                     oci-runtime-state-user)
  (group                    oci-runtime-state-group)
  (runtime-environment      oci-runtime-state-runtime-environment
                            (default #~()))
  (runtime-requirement      oci-runtime-state-runtime-requirement
                            (default '()))
  (runtime-extra-arguments  oci-runtime-state-runtime-extra-arguments
                            (default '())))

(define-record-type* <oci-state>
  oci-state
  make-oci-state
  oci-state?
  this-oci-state

  (networks                 oci-state-networks)
  (volumes                  oci-state-volumes)
  (containers               oci-state-containers)
  (networks-name            oci-state-networks-name
                            (default #f))
  (volumes-name             oci-state-volumes-name
                            (default #f))
  (networks-requirement     oci-state-networks-requirement
                            (default '()))
  (volumes-requirement      oci-state-volumes-requirement
                            (default '()))
  (containers-requirement   oci-state-containers-requirement
                            (default '())))

(define-record-type* <oci-container-invocation>
  oci-container-invocation
  make-oci-container-invocation
  oci-container-invocation?
  this-oci-container-invocation

  (runtime                  oci-container-invocation-runtime
                            (default 'docker))
  (runtime-cli              oci-container-invocation-runtime-cli)
  (name                     oci-container-invocation-name)
  (command                  oci-container-invocation-command
                            (default '()))
  (image-reference          oci-container-invocation-image-reference)
  (options                  oci-container-invocation-options
                            (default '()))
  (run-extra-arguments      oci-container-invocation-run-extra-arguments
                            (default '()))
  (runtime-extra-arguments  oci-container-invocation-runtime-extra-arguments
                            (default '())))

(define (oci-container-configuration->oci-container-invocation runtime-state
                                                               config)
  (oci-container-invocation
   (runtime (oci-runtime-state-runtime runtime-state))
   (runtime-cli (oci-runtime-state-runtime-cli runtime-state))
   (name
    (oci-container-shepherd-name runtime config))
   (command
    (mainline:oci-container-configuration-command config))
   (image-reference
    (oci-image-reference (mainline:oci-container-configuration-image config)))
   (options
    (oci-container-configuration->options config))
   (run-extra-arguments
    (mainline:oci-container-configuration-extra-arguments config))
   (runtime-extra-arguments
    (oci-runtime-state-runtime-extra-arguments runtime-state))))

(define* (oci-image-loader runtime-state name image tag #:key verbose?)
  "Return a file-like object that, once lowered, will evaluate to a program able
to load IMAGE through RUNTIME-CLI and to tag it with TAG afterwards."
  (let ((tarball (lower-oci-image name image)))
    (with-imported-modules (source-module-closure
                            '((oci build oci-containers))
                            #:select? oci-module-name?)
      (program-file
       (format #f "~a-image-loader" name)
       #~(begin
           (use-modules (oci build oci-containers))
           (oci-image-load '#$(oci-runtime-state-runtime runtime-state)
                           #$(oci-runtime-state-runtime-cli runtime-state)
                           #$tarball #$name #$tag
                           #:verbose? #$verbose?))))))

(define (oci-container-run-invocation container-invocation)
  "Return a list representing the OCI runtime
invocation for running containers."
  ;; run [OPTIONS] IMAGE [COMMAND] [ARG...]
  `(,(oci-container-invocation-runtime-cli container-invocation)
    ,@(oci-container-invocation-runtime-extra-arguments container-invocation)
    "run" "--rm"
    ,@(if (eq? (oci-container-invocation-runtime container-invocation)
               'podman)
          ;; This is because podman takes some time to
          ;; release container names.  --replace seems
          ;; to be required to be able to restart services.
          '("--replace")
          '())
    "--name" ,(oci-container-invocation-name container-invocation)
    ,@(oci-container-invocation-options container-invocation)
    ,@(oci-container-invocation-run-extra-arguments container-invocation)
    ,(oci-container-invocation-image-reference container-invocation)
    ,@(oci-container-invocation-command container-invocation)))

(define* (oci-container-entrypoint name invocation
                                   #:key verbose?
                                   (pre-script #~()))
  "Return a file-like object that, once lowered, will evaluate to the entrypoint
for the Shepherd service that will run INVOCATION."
  (program-file
   (string-append "oci-entrypoint-" name)
   (with-imported-modules (source-module-closure
                           '((oci build oci-containers))
                           #:select? oci-module-name?)
     #~(begin
         (use-modules (oci build oci-containers)
                      (srfi srfi-1))
         (oci-container-execlp
          (list #$@invocation)
          #:verbose? #$verbose?
          #:pre-script
          (lambda _
            (when (and #$verbose?
                       (zero? (length '(#$@pre-script))))
              (format #t "No pre script to run..."))
            #$@pre-script))))))

(define* (oci-container-shepherd-service state runtime-state config
                                         #:key verbose?
                                         networks?
                                         volumes?)
  "Return a Shepherd service object that will run the OCI container represented
by CONFIG through RUNTIME-CLI."

  (define shepherd-actions
    (mainline:oci-container-configuration-shepherd-actions config))
  (define auto-start?
    (mainline:oci-container-configuration-auto-start? config))
  (define oci-container-user
    (oci-runtime-state-user runtime-state))
  (define oci-container-group
    (oci-runtime-state-group runtime-state))
  (define host-environment
    (mainline:oci-container-configuration-host-environment config))
  (define log-file
    (mainline:oci-container-configuration-log-file config))
  (define requirement
    (mainline:oci-container-configuration-requirement config))
  (define respawn?
    (mainline:oci-container-configuration-respawn? config))
  (define image
    (mainline:oci-container-configuration-image config))

    (define runtime (oci-runtime-state-runtime runtime-state))
    (define runtime-cli (oci-runtime-state-runtime-cli runtime-state))
    (define image-reference (oci-image-reference image))
    (define shepherd-name (oci-container-shepherd-name runtime config))
    (define networks-service
      (if networks?
          (list
           (string->symbol
            (oci-state-networks-name state)))
          '()))
    (define volumes-service
     (if volumes?
         (list
          (string->symbol
           (oci-state-volumes-name state)))
         '()))
    (define oci-container-requirement
      (append requirement
              (oci-state-containers-requirement state)
              (oci-runtime-state-runtime-requirement runtime-state)
              networks-service
              volumes-service))
    (define environment-variables
      #~(append
         (list #$@host-environment)
         (list #$@(oci-runtime-state-runtime-environment runtime-state))))
    (define invocation
      (oci-container-run-invocation
       (oci-container-configuration->oci-container-invocation
        runtime-state config)))
    (define* (container-action command)
      #~(lambda _
          (fork+exec-command
           (list #$@command)
           #$@(if oci-container-user
                  (list #:user oci-container-user)
                  '())
           #$@(if oci-container-group
                  (list #:group oci-container-group)
                  '())
           #$@(if (maybe-value-set? log-file)
                  (list #:log-file log-file)
                  '())
           #$@(if (and oci-container-user (eq? runtime 'podman))
                  (list #:directory
                        #~(passwd:dir
                           (getpwnam #$oci-container-user)))
                  '())
           #:environment-variables
           #$environment-variables)))
    (define start-entrypoint
      (oci-container-entrypoint
       shepherd-name invocation
       #:verbose? verbose?
       #:pre-script
       (if (mainline:oci-image? image)
           #~((system*
               #$(oci-image-loader
                  runtime-state shepherd-name image
                  image-reference
                  #:verbose? verbose?)))
           #~())))

    (shepherd-service (provision `(,(string->symbol shepherd-name)))
                      (requirement oci-container-requirement)
                      (respawn? respawn?)
                      (auto-start? auto-start?)
                      (documentation
                       (string-append
                        (oci-runtime-name runtime)
                        " backed Shepherd service for "
                        (if (mainline:oci-image? image) shepherd-name image) "."))
                      (start
                       (container-action
                        (list start-entrypoint)))
                      (stop
                       (container-action
                        (list
                         (oci-container-entrypoint
                          shepherd-name
                          (list runtime-cli "rm" "-f" shepherd-name)
                          #:verbose? verbose?))))
                      (actions
                       (append
                        (list
                         (oci-command-line-shepherd-action
                          shepherd-name
                          #~(string-join (list #$@invocation) " "))
                         (oci-entrypoint-shepherd-action
                          shepherd-name start-entrypoint))
                        (if (mainline:oci-image? image)
                            '()
                            (list
                             (shepherd-action
                              (name 'pull)
                              (documentation
                               (format #f "Pull ~a's image (~a)."
                                       shepherd-name image))
                              (procedure
                               (container-action
                                (list
                                 (oci-container-entrypoint
                                  shepherd-name (list runtime-cli "pull" image)
                                  #:verbose? verbose?)))))))
                        shepherd-actions))))

(define (oci-object-create-invocation object runtime-cli name options
                                      runtime-extra-arguments
                                      create-extra-arguments)
  "Return a gexp that, upon lowering, will evaluate to the OCI runtime
invocation for creating networks and volumes."
  ;; network|volume create [options] [NAME]
  #~(list #$runtime-cli #$@runtime-extra-arguments #$object "create"
          #$@options #$@create-extra-arguments #$name))

(define (format-oci-invocations invocations)
  "Return a gexp that, upon lowering, will evaluate to a formatted message
containing the INVOCATIONS that the OCI runtime will execute to provision
networks or volumes."
  #~(string-join (map (lambda (i) (string-join i " "))
                      (list #$@invocations))
                 "\n"))

(define* (oci-object-create-script object runtime runtime-cli invocations
                                   #:key verbose?)
  "Return a file-like object that, once lowered, will evaluate to a program able
to create OCI networks and volumes through RUNTIME-CLI."
  (define runtime-string (symbol->string runtime))
  (define runtime-name (oci-runtime-name runtime))
  (with-imported-modules (source-module-closure
                          '((oci build oci-containers))
                          #:select? oci-module-name?)

    (program-file
     (string-append runtime-string "-" object "s-create.scm")
     #~(begin
         (use-modules (oci build oci-containers))
         (oci-object-create '#$runtime #$runtime-cli #$runtime-name
                            #$object (list #$@invocations)
                            #:verbose? #$verbose?)))))

(define* (oci-object-shepherd-service object runtime-state name
                                      oci-state-requirement invocations
                                      #:key verbose?)
  "Return a Shepherd service object that will provision the OBJECTs represented
by INVOCATIONS through RUNTIME-STATE."
  (match-record runtime-state <oci-runtime-state>
                (runtime runtime-cli runtime-requirement user group
                 runtime-environment)
    (define entrypoint
      (oci-object-create-script
       object runtime runtime-cli invocations #:verbose? verbose?))
    (define requirement
      (append runtime-requirement oci-state-requirement))

    (shepherd-service (provision (list (string->symbol name)))
                      (requirement requirement)
                      (one-shot? #t)
                      (documentation
                       (string-append
                        (oci-runtime-name runtime) " " object
                        " provisioning service"))
                      (start
                       #~(lambda _
                           (fork+exec-command
                            (list #$entrypoint)
                            #$@(if user (list #:user user) '())
                            #$@(if group (list #:group group) '())
                            #:environment-variables
                            (list #$@runtime-environment))))
                      (actions
                       (list
                        (oci-entrypoint-shepherd-action
                         name entrypoint)
                        (oci-command-line-shepherd-action
                         name (format-oci-invocations invocations)))))))

(define* (oci-networks-shepherd-service state runtime-state
                                        #:key verbose?)
  "Return a Shepherd service object that will create the networks represented
in STATE."
  (define runtime-cli
    (oci-runtime-state-runtime-cli runtime-state))
  (define invocations
    (map
     (lambda (network)
       (oci-object-create-invocation
        "network" runtime-cli
        (oci-network-configuration-name network)
        (oci-network-configuration->options network)
        (oci-runtime-state-runtime-extra-arguments runtime-state)
        (oci-network-configuration-extra-arguments network)))
     (oci-state-networks state)))

  (oci-object-shepherd-service
   "network" runtime-state (oci-state-networks-name state)
   (oci-state-networks-requirement state)
   invocations #:verbose? verbose?))

(define* (oci-volumes-shepherd-service state runtime-state
                                       #:key verbose?)
  "Return a Shepherd service object that will create the volumes represented
in STATE."
  (define runtime-cli
    (oci-runtime-state-runtime-cli runtime-state))
  (define invocations
    (map
     (lambda (volume)
       (oci-object-create-invocation
        "volume" runtime-cli
        (oci-volume-configuration-name volume)
        (oci-volume-configuration->options volume)
        (oci-runtime-state-runtime-extra-arguments runtime-state)
        (oci-volume-configuration-extra-arguments volume)))
     (oci-state-volumes state)))

  (oci-object-shepherd-service
   "volume" runtime-state (oci-state-volumes-name state)
   (oci-state-volumes-requirement state)
   invocations #:verbose? verbose?))

(define (oci-service-accounts config)
  (define user (oci-configuration-user config))
  (define maybe-group (oci-configuration-group config))
  (define runtime (oci-configuration-runtime config))
  (list (user-account
         (name user)
         (comment "OCI services account")
         (group "users")
         (supplementary-groups
          (list (oci-runtime-group runtime maybe-group)))
         (system? (eq? 'docker runtime))
         (home-directory (if (eq? 'podman runtime)
                             (string-append "/home/" user)
                             "/var/empty"))
         (create-home-directory? (eq? 'podman runtime))
         (shell (file-append shadow "/sbin/nologin")))))

(define* (oci-state->shepherd-services state runtime-state #:key verbose?)
  "Returns a list of Shepherd services based on the input OCI state."
  (define networks?
    (> (length (oci-state-networks state)) 0))
  (define volumes?
    (> (length (oci-state-volumes state)) 0))
  (append
   (map
    (lambda (c)
      (oci-container-shepherd-service
       state runtime-state c
       #:verbose? verbose?
       #:volumes? volumes?
       #:networks? networks?))
    (oci-state-containers state))
   (if networks?
       (list
        (oci-networks-shepherd-service
         state runtime-state
         #:verbose? verbose?))
       '())
   (if volumes?
       (list
        (oci-volumes-shepherd-service
         state runtime-state
         #:verbose? verbose?))
       '())))

(define* (oci-configuration->oci-runtime-state config #:key verbose?)
  (define runtime
    (oci-configuration-runtime config))
  (define home-service?
    (oci-configuration-home-service? config))
  (define runtime-cli
    (if home-service?
        (oci-runtime-home-cli config)
        (oci-runtime-system-cli config)))
  (define user
    (if home-service?
        #f
        (oci-configuration-user config)))
  (define group
    (if home-service?
        #f
        (if (eq? runtime 'podman)
            #~(group:name
               (getgrgid
                (passwd:gid
                 (getpwnam #$user))))
            (oci-runtime-group config (oci-configuration-group config)))))
  (define runtime-requirement
    (if home-service?
        '()
        (oci-runtime-system-requirement runtime)))
  (define runtime-environment
    (if home-service?
        #~()
        (oci-runtime-system-environment runtime user)))
  (oci-runtime-state
   (runtime runtime)
   (runtime-cli runtime-cli)
   (user user)
   (group group)
   (runtime-extra-arguments
    (oci-configuration-runtime-extra-arguments config))
   (runtime-environment runtime-environment)
   (runtime-requirement runtime-requirement)))

(define (oci-configuration->oci-state config)
  (define runtime
    (oci-configuration-runtime config))
  (define home-service?
    (oci-configuration-home-service? config))
  (define networks-name
    (if home-service?
        (oci-networks-home-shepherd-name runtime)
        (oci-networks-shepherd-name runtime)))
  (define volumes-name
    (if home-service?
        (oci-volumes-home-shepherd-name runtime)
        (oci-volumes-shepherd-name runtime)))
  (define networks-requirement
    (if home-service?
        '()
        '(networking)))
  (oci-state
   (containers (oci-configuration-containers config))
   (networks (oci-configuration-networks config))
   (volumes (oci-configuration-volumes config))
   (networks-name networks-name)
   (volumes-name volumes-name)
   (networks-requirement networks-requirement)))

(define (oci-configuration->shepherd-services config)
  (let* ((verbose? (oci-configuration-verbose? config))
         (state (oci-configuration->oci-state config))
         (runtime-state
          (oci-configuration->oci-runtime-state config #:verbose? verbose?)))
    (oci-state->shepherd-services state runtime-state #:verbose? verbose?)))

(define (oci-service-subids config)
  "Return a subids-extension record representing subuids and subgids required by
the rootless Podman backend."
  (define (find-duplicates subids)
    (let loop ((names '())
               (subids subids))
      (if (null? names)
          names
          (loop
           (let ((name (subid-range-name (car subids))))
             (if (member name names)
                 (raise
                  (formatted-message
                   (G_ "Duplicated subid-range: ~a. subid-ranges names should be
unique, please remove the duplicate.") name))
                 (cons name names)))
           (cdr subids)))))

  (define runtime
    (oci-configuration-runtime config))
  (define user
    (oci-configuration-user config))

  (define subgids (oci-configuration-subgids-range config))
  (find-duplicates subgids)

  (define subuids (oci-configuration-subuids-range config))
  (find-duplicates subgids)

  (define container-users
    (filter (lambda (range) (not (string=? (subid-range-name range) user)))
            (map (lambda (container)
                   (subid-range
                    (name
                     (mainline:oci-container-configuration-user container))))
                 (oci-configuration-containers config))))
  (define subgid-ranges
    (cons
     (if (maybe-value-set? subgids)
         subgids
         (subid-range (name user)))
     container-users))
  (define subuid-ranges
    (cons
     (if (maybe-value-set? subuids)
         subuids
         (subid-range (name user)))
     container-users))

  (if (eq? 'podman runtime)
      (subids-extension
       (subgids
        subgid-ranges)
       (subuids
        subuid-ranges))
      (subids-extension)))

(define (oci-objects-merge-lst a b object get-name)
  (define (contains? value lst)
    (member value (map get-name lst)))
  (let loop ((merged '())
             (lst (append a b)))
    (if (null? lst)
        merged
        (loop
         (let ((element (car lst)))
           (when (contains? element merged)
             (raise
              (formatted-message
               (G_ "Duplicated ~a: ~a. Names of ~a should be unique, please
remove the duplicate.") object (get-name element) object)))
           (cons element merged))
         (cdr lst)))))

(define (oci-extension-merge a b)
  (oci-extension
   (containers (oci-objects-merge-lst
                (oci-extension-containers a)
                (oci-extension-containers b)
                "container"
                (lambda (config)
                  (define maybe-name
                    (mainline:oci-container-configuration-provision config))
                  (if (maybe-value-set? maybe-name)
                      maybe-name
                      (oci-image->container-name
                       (mainline:oci-container-configuration-image config))))))
   (networks (oci-objects-merge-lst
              (oci-extension-networks a)
              (oci-extension-networks b)
              "network"
              oci-network-configuration-name))
   (volumes (oci-objects-merge-lst
             (oci-extension-volumes a)
             (oci-extension-volumes b)
             "volume"
             oci-volume-configuration-name))))

(define (oci-service-profile runtime runtime-cli)
  `(,bash-minimal
    ,@(if (string? runtime-cli)
          '()
          (list
           (cond
            ((maybe-value-set? runtime-cli)
             runtime-cli)
            ((eq? 'podman runtime)
             podman)
            (else
             docker-cli))))))

(define (oci-configuration-extend config extension)
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
             oci-volume-configuration-name))))

(define oci-service-type
  (service-type
   (name 'oci)
   (extensions
    (list
     (service-extension profile-service-type
                        (lambda (config)
                          (let ((runtime-cli
                                 (oci-configuration-runtime-cli config))
                                (runtime
                                 (oci-configuration-runtime config)))
                            (oci-service-profile runtime runtime-cli))))
     (service-extension subids-service-type
                        oci-service-subids)
     (service-extension account-service-type
                        oci-service-accounts)
     (service-extension shepherd-root-service-type
                        oci-configuration->shepherd-services)))
   ;; Concatenate OCI object lists.
   (compose (lambda (args)
              (fold oci-extension-merge
                    (oci-extension)
                    args)))
   (extend oci-configuration-extend)
   (default-value (oci-configuration))
   (description
    "This service implements the provisioning of OCI objects such
as containers, networks and volumes.")))

(define oci-container-service-type
  (service-type (name 'oci-container)
                (extensions
                 (list
                  (service-extension oci-service-type
                                     (lambda (containers)
                                       (warning
                                        (G_
                                         "'oci-container-service-type' is deprecated, use 'oci-service-type' instead~%"))
                                       (oci-extension
                                        (containers containers))))))
                (default-value '())
                (extend append)
                (compose concatenate)
                (description
                 "This service allows the management of OCI
containers as Shepherd services.")))
