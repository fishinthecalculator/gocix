;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024, 2025 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (oci services containers)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages containers)
  #:use-module (gnu packages docker)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module ((gnu services docker) #:prefix mainline:)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)

  #:export (oci-container-service-type

            list-of-oci-containers?
            list-of-oci-networks?
            list-of-oci-volumes?

            %oci-supported-runtimes
            oci-runtime-system-environment
            oci-runtime-system-extra-arguments
            oci-runtime-system-group
            oci-runtime-system-requirement
            oci-runtime-cli
            oci-sanitize-runtime
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
;;; https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/services/docker.scm
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
  (if (eq? maybe-group #f)
      (if (eq? 'podman runtime)
          "cgroup"
          "docker")
      maybe-group))

(define (oci-sanitize-runtime value)
  (unless (member value %oci-supported-runtimes)
    (raise
     (formatted-message
      (G_ "OCI runtime must be a symbol and one of ~a,
but ~a was found") %oci-supported-runtimes value)))
  value)

(define oci-sanitize-mixed-list
  (@@ (gnu services docker) oci-sanitize-mixed-list))

(define (oci-sanitize-labels value)
  ;; Expected spec format:
  ;; '(("foo" . "bar") "foo=bar")
  (oci-sanitize-mixed-list "labels" value "="))

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
   (list '())
   "The list of labels that will be used to tag the current volume."
   (sanitizer oci-sanitize-labels))
  (extra-arguments
   (list '())
   "A list of strings, gexps or file-like objects that will be directly passed
to the @command{docker volume create} or @command{podman volume create}
invokation."
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
   (list '())
   "The list of labels that will be used to tag the current volume."
   (sanitizer oci-sanitize-labels))
  (extra-arguments
   (list '())
   "A list of strings, gexps or file-like objects that will be directly passed
to the @command{docker network create} or @command{podman network create}
invokation."
   (sanitizer oci-sanitize-extra-arguments)))

(define (list-of-oci-networks? value)
  (list-of-oci-records? "networks" oci-network-configuration? value))

(define-record-type* <oci-configuration>
  oci-configuration
  make-oci-configuration
  oci-configuration?
  this-oci-configuration

  (runtime                  oci-configuration-runtime
                            (default 'docker))
  (runtime-cli              oci-configuration-runtime-cli
                            (default #f))                               ; package or string
  (runtime-extra-arguments  oci-configuration-runtime-extra-arguments   ; strings or gexps
                            (default '()))                              ; or file-like objects
  (user                     oci-configuration-user
                            (default "oci-container"))
  (group                    oci-configuration-group                     ; string
                            (default #f))
  (subuids-range            oci-configuration-subuids-range             ; subid-range
                            (default #f))
  (subgids-range            oci-configuration-subgids-range             ; subid-range
                            (default #f))
  (containers               oci-configuration-containers                ; oci-container-configurations
                            (default '()))
  (networks                 oci-configuration-networks                  ; oci-network-configurations
                            (default '()))
  (volumes                  oci-configuration-volumes                   ; oci-volume-configurations
                            (default '()))
  (verbose?                 oci-configuration-verbose?
                            (default #f))
  (home-service?            oci-configuration-home-service?
                            (default for-home?) (innate)))

(define (package-or-string? value)
  (or (package? value) (string? value)))

(define (oci-configuration-valid? config)
  (define runtime-cli
    (oci-configuration-runtime-cli config))
  (define group
    (oci-configuration-group config))
  (define subuids-range
    (oci-configuration-subuids-range config))
  (define subgids-range
    (oci-configuration-subgids-range config))
  (and
   (symbol?
    (oci-sanitize-runtime (oci-configuration-runtime config)))
   (or (eq? runtime-cli #f)
       (package-or-string? runtime-cli))
   (list? (oci-configuration-runtime-extra-arguments config))
   (string? (oci-configuration-user config))
   (or (eq? group #f)
       (string? group))
   (or (eq? subuids-range #f)
       (subid-range? subuids-range))
   (or (eq? subgids-range #f)
       (subid-range? subgids-range))
   (list-of-oci-containers?
    (oci-configuration-containers config))
   (list-of-oci-networks?
    (oci-configuration-networks config))
   (list-of-oci-volumes?
    (oci-configuration-volumes config))
   (boolean?
    (oci-configuration-verbose? config))
   (boolean?
    (oci-configuration-home-service? config))))

(define (oci-runtime-system-environment runtime user)
  (if (eq? runtime 'podman)
      (list
       #~(string-append
          "HOME=" (passwd:dir (getpwnam #$user))))
      #~()))

(define (oci-runtime-system-group runtime user group)
  (if (eq? runtime 'podman)
      #~(group:name
         (getgrgid
          (passwd:gid
           (getpwnam #$user))))
      group))

(define (oci-runtime-cli runtime runtime-cli path)
  "Return a gexp that, when lowered, evaluates to the file system path of the OCI
runtime command requested by the user."
  (if (string? runtime-cli)
      ;; It is a user defined absolute path
      runtime-cli
      #~(string-append
         #$(if (eq? runtime-cli #f)
               path
               runtime-cli)
         #$(if (eq? 'podman runtime)
               "/bin/podman"
               "/bin/docker"))))

(define* (oci-runtime-system-cli config #:key (path "/run/current-system/profile"))
  (let ((runtime-cli
         (oci-configuration-runtime-cli config))
        (runtime
         (oci-configuration-runtime config)))
    (oci-runtime-cli runtime runtime-cli path)))

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

(define (oci-object-command-shepherd-action object-name invokation)
  "Return a Shepherd action printing a given INVOKATION of an OCI command for the
given OBJECT-NAME."
  (shepherd-action
   (name 'command-line)
   (documentation
    (format #f "Prints ~a's OCI runtime command line invokation."
            object-name))
   (procedure
    #~(lambda _
        (format #t "~a~%" #$invokation)))))

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
  (@@ (gnu services docker) lower-oci-image))

(define (oci-object-exists? runtime runtime-cli object verbose?)
  #~(lambda* (name #:key (format-string "{{.Name}}"))
      (use-modules (ice-9 format)
                   (ice-9 match)
                   (ice-9 popen)
                   (ice-9 rdelim)
                   (srfi srfi-1))

      (define (read-lines file-or-port)
        (define (loop-lines port)
          (let loop ((lines '()))
            (match (read-line port)
              ((? eof-object?)
               (reverse lines))
              (line
               (loop (cons line lines))))))

        (if (port? file-or-port)
            (loop-lines file-or-port)
            (call-with-input-file file-or-port
              loop-lines)))

      #$(if (eq? runtime 'podman)
            #~(let ((command
                     (list #$runtime-cli
                           #$object "exists" name)))
                (when #$verbose?
                  (format #t "Running~{ ~a~}~%" command))
                (define exit-code (status:exit-val (apply system* command)))
                (when #$verbose?
                  (format #t "Exit code: ~a~%" exit-code))
                (equal? EXIT_SUCCESS exit-code))
            #~(let ((command
                     (string-append #$runtime-cli
                                    " " #$object " ls --format "
                                    "\"" format-string "\"")))
                (when #$verbose?
                  (format #t "Running ~a~%" command))
                (member name (read-lines (open-input-pipe command)))))))

(define* (oci-image-loader runtime runtime-cli name image tag #:key (verbose? #f))
  "Return a file-like object that, once lowered, will evaluate to a program able
to load IMAGE through RUNTIME-CLI and to tag it with TAG afterwards."
  (let ((tarball (lower-oci-image name image)))
    (with-imported-modules '((guix build utils))
      (program-file
       (format #f "~a-image-loader" name)
       #~(begin
           (use-modules (guix build utils)
                        (ice-9 match)
                        (ice-9 popen)
                        (ice-9 rdelim)
                        (srfi srfi-1))
           (define object-exists?
             #$(oci-object-exists? runtime runtime-cli "image" verbose?))
           (define load-command
             (string-append #$runtime-cli
                            " load -i " #$tarball))

           (if (object-exists? #$tag #:format-string "{{.Repository}}:{{.Tag}}")
               (format #t "~a image already exists, skipping.~%" #$tag)
               (begin
                 (format #t "Loading image for ~a from ~a...~%" #$name #$tarball)
                 (when #$verbose?
                   (format #t "Running ~a~%" load-command))
                 (let ((line (read-line
                              (open-input-pipe load-command))))
                   (unless (or (eof-object? line)
                               (string-null? line))
                     (format #t "~a~%" line)
                     (let* ((repository&tag
                             (string-drop line
                                          (string-length
                                           "Loaded image: ")))
                            (tag-command
                             (list #$runtime-cli "tag" repository&tag #$tag))
                            (drop-old-tag-command
                             (list #$runtime-cli "image" "rm" "-f" repository&tag)))

                       (unless (string=? repository&tag #$tag)
                         (when #$verbose?
                           (format #t "Running~{ ~a~}~%" tag-command))

                         (let ((exit-code
                                (status:exit-val (apply system* tag-command))))
                           (format #t "Tagged ~a with ~a...~%" #$tarball #$tag)

                           (when #$verbose?
                             (format #t "Exit code: ~a~%" exit-code))

                           (when (equal? EXIT_SUCCESS exit-code)
                             (when #$verbose?
                               (format #t "Running~{ ~a~}~%" drop-old-tag-command))
                             (let ((drop-exit-code
                                    (status:exit-val (apply system* drop-old-tag-command))))
                               (when #$verbose?
                                 (format #t "Exit code: ~a~%" drop-exit-code))))))))))))))))

(define (oci-container-run-invokation runtime runtime-cli name command image-reference
                                      options runtime-extra-arguments run-extra-arguments)
  "Return a list representing the OCI runtime
invokation for running containers."
  ;; run [OPTIONS] IMAGE [COMMAND] [ARG...]
  `(,runtime-cli ,@runtime-extra-arguments "run" "--rm"
    ,@(if (eq? runtime 'podman)
          '("--replace")
          '())
    "--name" ,name
    ,@options ,@run-extra-arguments
    ,image-reference ,@command))

(define* (oci-container-entrypoint runtime runtime-cli name image image-reference
                                   invocation #:key (verbose? #f) (pre-script #~()))
  "Return a file-like object that, once lowered, will evaluate to the entrypoint
for the Shepherd service that will run IMAGE through RUNTIME-CLI."
  (program-file
   (string-append "oci-entrypoint-" name)
   #~(begin
       (use-modules (ice-9 format)
                    (srfi srfi-1))
       (when #$verbose?
         (format #t "Running in verbose mode...~%"))
       (define invocation (list #$@invocation))
       #$@pre-script
       (when #$verbose?
         (format #t "Running~{ ~a~}~%" invocation))
       (apply execlp `(,(first invocation) ,@invocation)))))

(define* (oci-container-shepherd-service runtime runtime-cli config
                                         #:key
                                         (runtime-environment #~())
                                         (runtime-extra-arguments '())
                                         (oci-requirement '())
                                         (user #f)
                                         (group #f)
                                         (verbose? #f))
  "Return a Shepherd service object that will run the OCI container represented
by CONFIG through RUNTIME-CLI."
  (let* ((actions (mainline:oci-container-configuration-shepherd-actions config))
         (auto-start?
          (mainline:oci-container-configuration-auto-start? config))
         (host-environment
          (mainline:oci-container-configuration-host-environment config))
         (command (mainline:oci-container-configuration-command config))
         (log-file (mainline:oci-container-configuration-log-file config))
         (requirement (mainline:oci-container-configuration-requirement config))
         (respawn?
          (mainline:oci-container-configuration-respawn? config))
         (image (mainline:oci-container-configuration-image config))
         (image-reference (oci-image-reference image))
         (options (oci-container-configuration->options config))
         (name
          (oci-container-shepherd-name runtime config))
         (extra-arguments
          (mainline:oci-container-configuration-extra-arguments config))
         (invokation
          (oci-container-run-invokation
           runtime runtime-cli name command image-reference
           options runtime-extra-arguments extra-arguments))
         (container-action
          (lambda* (command #:key (environment-variables #f))
            #~(lambda _
                (fork+exec-command
                 (list #$@command)
                 #$@(if user (list #:user user) '())
                 #$@(if group (list #:group group) '())
                 #$@(if (maybe-value-set? log-file)
                        (list #:log-file log-file)
                        '())
                 #$@(if (and user (eq? runtime 'podman))
                        (list #:directory
                              #~(passwd:dir (getpwnam #$user)))
                        '())
                 #$@(if environment-variables
                        (list #:environment-variables
                              environment-variables)
                        '()))))))

    (shepherd-service (provision `(,(string->symbol name)))
                      (requirement `(,@oci-requirement
                                     ,@requirement))
                      (respawn? respawn?)
                      (auto-start? auto-start?)
                      (documentation
                       (string-append
                        (oci-runtime-name runtime) " backed Shepherd service for "
                        (if (mainline:oci-image? image) name image) "."))
                      (start
                       (container-action
                        (list (oci-container-entrypoint
                               runtime runtime-cli name image image-reference
                               invokation #:verbose? verbose?
                               #:pre-script
                               (if (mainline:oci-image? image)
                                   #~((system*
                                       #$(oci-image-loader
                                          runtime runtime-cli name image
                                          image-reference #:verbose? verbose?)))
                                   #~())))
                        #:environment-variables
                        #~(append
                           (list #$@host-environment)
                           (list #$@runtime-environment))))
                      (stop
                       (container-action
                        (list
                         (oci-container-entrypoint
                          runtime runtime-cli name image image-reference
                          (list runtime-cli "rm" "-f" name)
                          #:verbose? verbose?))
                        #:environment-variables
                        #~(append
                           (list #$@host-environment)
                           (list #$@runtime-environment))))
                      (actions
                       (append
                        (list
                         (oci-object-command-shepherd-action
                          name #~(string-join (list #$@invokation) " ")))
                        (if (mainline:oci-image? image)
                            '()
                            (list
                             (let ((service-name name))
                               (shepherd-action
                                (name 'pull)
                                (documentation
                                 (format #f "Pull ~a's image (~a)."
                                         service-name image))
                                (procedure
                                 (container-action
                                  (list
                                   (oci-container-entrypoint
                                    runtime runtime-cli service-name image image-reference
                                    (list runtime-cli "pull" image)
                                    #:verbose? verbose?))
                                  #:environment-variables
                                  #~(append
                                     (list #$@host-environment)
                                     (list #$@runtime-environment))))))))
                        actions)))))

(define (oci-object-create-invokation object runtime-cli name options
                                      runtime-extra-arguments
                                      create-extra-arguments)
  "Return a gexp that, upon lowering, will evaluate to the OCI runtime
invokation for creating networks and volumes."
  ;; network|volume create [options] [NAME]
  #~(list #$runtime-cli #$@runtime-extra-arguments #$object "create"
          #$@options #$@create-extra-arguments #$name))

(define (format-oci-invokations invokations)
  "Return a gexp that, upon lowering, will evaluate to a formatted message
containing the INVOKATIONS that the OCI runtime will execute to provision
networks or volumes."
  #~(string-join (map (lambda (i) (string-join i " "))
                      (list #$@invokations))
                 "\n"))

(define* (oci-object-create-script object runtime runtime-cli invokations
                                   #:key (verbose? #f))
  "Return a file-like object that, once lowered, will evaluate to a program able
to create OCI networks and volumes through RUNTIME-CLI."
  (define runtime-string (symbol->string runtime))
  (program-file
   (string-append runtime-string "-" object "s-create.scm")
   #~(begin
       (use-modules (ice-9 format)
                    (ice-9 match)
                    (ice-9 popen)
                    (ice-9 rdelim)
                    (srfi srfi-1))

       (define object-exists?
         #$(oci-object-exists? runtime runtime-cli object verbose?))

       (for-each
        (lambda (invokation)
          (define name (last invokation))
          (if (object-exists? name)
              (format #t "~a ~a ~a already exists, skipping creation.~%"
                      #$(oci-runtime-name runtime) name #$object)
              (begin
                (when #$verbose?
                  (format #t "Running~{ ~a~}~%" invokation))
                (let ((exit-code (status:exit-val (apply system* invokation))))
                  (when #$verbose?
                    (format #t "Exit code: ~a~%" exit-code))))))
        (list #$@invokations)))))

(define* (oci-object-shepherd-service object runtime runtime-cli name requirement invokations
                                      #:key
                                      (runtime-environment #~())
                                      (user #f)
                                      (group #f)
                                      (verbose? #f))
  "Return a Shepherd service object that will create the OBJECTs represented
by INVOKATIONS through RUNTIME-CLI."
  (shepherd-service (provision `(,(string->symbol name)))
                    (requirement requirement)
                    (one-shot? #t)
                    (documentation
                     (string-append
                      (oci-runtime-name runtime) " " object
                      " provisioning service"))
                    (start
                     #~(lambda _
                         (fork+exec-command
                          (list
                           #$(oci-object-create-script
                              object runtime runtime-cli
                              invokations
                              #:verbose? verbose?))
                          #$@(if user (list #:user user) '())
                          #$@(if group (list #:group group) '())
                          #:environment-variables
                          (list #$@runtime-environment))))
                    (actions
                     (list
                      (oci-object-command-shepherd-action
                       name (format-oci-invokations invokations))))))

(define* (oci-networks-shepherd-service runtime runtime-cli name networks
                                        #:key (user #f) (group #f) (verbose? #f)
                                        (runtime-extra-arguments '())
                                        (runtime-environment #~())
                                        (runtime-requirement '()))
  "Return a Shepherd service object that will create the networks represented
in CONFIG."
  (let ((invokations
         (map
          (lambda (network)
            (oci-object-create-invokation
             "network" runtime-cli
             (oci-network-configuration-name network)
             (oci-network-configuration->options network)
             runtime-extra-arguments
             (oci-network-configuration-extra-arguments network)))
          networks)))

    (oci-object-shepherd-service
     "network" runtime runtime-cli name
     runtime-requirement invokations
     #:user user #:group group #:runtime-environment runtime-environment
     #:verbose? verbose?)))

(define* (oci-volumes-shepherd-service runtime runtime-cli name volumes
                                       #:key (user #f) (group #f) (verbose? #f)
                                       (runtime-extra-arguments '())
                                       (runtime-environment #~())
                                       (runtime-requirement '()))
  "Return a Shepherd service object that will create the volumes represented
in CONFIG."
  (let ((invokations
         (map
          (lambda (volume)
            (oci-object-create-invokation
             "volume" runtime-cli
             (oci-volume-configuration-name volume)
             (oci-volume-configuration->options volume)
             runtime-extra-arguments
             (oci-volume-configuration-extra-arguments volume)))
          volumes)))

    (oci-object-shepherd-service
     "volume" runtime runtime-cli name runtime-requirement invokations
     #:user user #:group group #:runtime-environment runtime-environment
     #:verbose? verbose?)))

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

(define* (oci-state->shepherd-services runtime runtime-cli containers networks volumes
                                       #:key (user #f) (group #f) (verbose? #f)
                                       (networks-name #f) (volumes-name #f)
                                       (runtime-extra-arguments '())
                                       (runtime-environment #~())
                                       (runtime-requirement '())
                                       (containers-requirement '())
                                       (networks-requirement '())
                                       (volumes-requirement '()))
  (let* ((networks-name
          (if (string? networks-name)
              networks-name
              (oci-networks-shepherd-name runtime)))
         (networks?
          (> (length networks) 0))
         (networks-service
          (if networks?
              (list
               (string->symbol networks-name))
              '()))
         (volumes-name
          (if (string? volumes-name)
              volumes-name
              (oci-volumes-shepherd-name runtime)))
         (volumes?
          (> (length volumes) 0))
         (volumes-service
          (if volumes?
              (list (string->symbol volumes-name))
              '())))
    (append
     (map
      (lambda (c)
        (oci-container-shepherd-service
         runtime runtime-cli c
         #:user user
         #:group group
         #:runtime-environment runtime-environment
         #:runtime-extra-arguments runtime-extra-arguments
         #:oci-requirement
         (append containers-requirement
                 runtime-requirement
                 networks-service
                 volumes-service)
         #:verbose? verbose?))
      containers)
     (if networks?
         (list
          (oci-networks-shepherd-service
           runtime runtime-cli
           networks-name networks
           #:user user #:group group
           #:runtime-extra-arguments runtime-extra-arguments
           #:runtime-environment runtime-environment
           #:runtime-requirement (append networks-requirement
                                         runtime-requirement)
           #:verbose? verbose?))
         '())
     (if volumes?
         (list
          (oci-volumes-shepherd-service
           runtime runtime-cli
           volumes-name volumes
           #:user user #:group group
           #:runtime-extra-arguments runtime-extra-arguments
           #:runtime-environment runtime-environment
           #:runtime-requirement (append runtime-requirement
                                         volumes-requirement)
           #:verbose? verbose?))
         '()))))

(define (oci-configuration->shepherd-services config)
  (let* ((runtime (oci-configuration-runtime config))
         (system-runtime-cli
          (oci-runtime-system-cli config))
         (home-runtime-cli
          (oci-runtime-home-cli config))
         (runtime-extra-arguments
          (oci-configuration-runtime-extra-arguments config))
         (containers (oci-configuration-containers config))
         (networks (oci-configuration-networks config))
         (volumes (oci-configuration-volumes config))
         (user (oci-configuration-user config))
         (group (oci-runtime-group
                 runtime (oci-configuration-group config)))
         (verbose? (oci-configuration-verbose? config))
         (home-service?
          (oci-configuration-home-service? config)))
    (if home-service?
        (oci-state->shepherd-services runtime home-runtime-cli containers networks volumes
                                      #:verbose? verbose?
                                      #:networks-name
                                      (oci-networks-home-shepherd-name runtime)
                                      #:volumes-name
                                      (oci-volumes-home-shepherd-name runtime))
        (oci-state->shepherd-services runtime system-runtime-cli containers networks volumes
                                      #:user user
                                      #:group
                                      (oci-runtime-system-group runtime user group)
                                      #:verbose? verbose?
                                      #:runtime-extra-arguments
                                      runtime-extra-arguments
                                      #:runtime-environment
                                      (oci-runtime-system-environment runtime user)
                                      #:runtime-requirement
                                      (oci-runtime-system-requirement runtime)
                                      #:networks-requirement '(networking)))))

(define (oci-service-subids config)
  "Return a subids-extension record representing subuids and subgids required by
the rootless Podman backend."
  (define (delete-duplicate-ranges ranges)
    (delete-duplicates ranges
                       (lambda args
                         (apply string=? (map subid-range-name ranges)))))
  (define runtime
    (oci-configuration-runtime config))
  (define user
    (oci-configuration-user config))
  (define subgids (oci-configuration-subgids-range config))
  (define subuids (oci-configuration-subuids-range config))
  (define container-users
    (filter (lambda (range) (not (string=? (subid-range-name range) user)))
            (map (lambda (container)
                   (subid-range
                    (name
                     (mainline:oci-container-configuration-user container))))
                 (oci-configuration-containers config))))
  (define subgid-ranges
    (delete-duplicate-ranges
     (cons
      (if (eq? subgids #f)
          (subid-range (name user))
          subgids)
      container-users)))
  (define subuid-ranges
    (delete-duplicate-ranges
     (cons
      (if (eq? subuids #f)
          (subid-range (name user))
          subuids)
      container-users)))

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
               (G_ "Duplicated ~a: ~a. ~as names should be unique, please
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
                  (define maybe-name (mainline:oci-container-configuration-provision config))
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
            ((not (eq? runtime-cli #f))
             runtime-cli)
            ((eq? 'podman runtime)
             podman)
            (else
             docker-cli))))))

(define (oci-service-extension-wrap-validate extension)
  (lambda (config)
    (if (oci-configuration-valid? config)
        (extension config)
        (raise
         (formatted-message
          (G_ "Invalide oci-configuration ~a.") config)))))

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
  (service-type (name 'oci)
                (extensions
                 (list
                  (service-extension profile-service-type
                                     (oci-service-extension-wrap-validate
                                      (lambda (config)
                                        (let ((runtime-cli
                                               (oci-configuration-runtime-cli config))
                                              (runtime
                                               (oci-configuration-runtime config)))
                                          (oci-service-profile runtime runtime-cli)))))
                  (service-extension subids-service-type
                                     (oci-service-extension-wrap-validate
                                      oci-service-subids))
                  (service-extension account-service-type
                                     (oci-service-extension-wrap-validate
                                      oci-service-accounts))
                  (service-extension shepherd-root-service-type
                                     (oci-service-extension-wrap-validate
                                      oci-configuration->shepherd-services))))
                ;; Concatenate OCI object lists.
                (compose (lambda (args)
                           (fold oci-extension-merge
                                 (oci-extension)
                                 args)))
                (extend oci-configuration-extend)
                (default-value (oci-configuration))
                (description
                 "This service implements the provisioning of OCI object such
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
