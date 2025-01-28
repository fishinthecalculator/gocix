;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>

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
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)

  #:export (%oci-supported-runtimes
            oci-runtime-requirement
            oci-runtime-cli
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
            oci-configuration-user
            oci-configuration-group
            oci-container-configuration-subuids-range
            oci-container-configuration-subgids-range
            oci-configuration-containers
            oci-configuration-networks
            oci-configuration-volumes
            oci-configuration-verbose?

            oci-extension
            oci-extension?
            oci-extension-fields
            oci-extension-containers
            oci-extension-networks
            oci-extension-volumes

            oci-container-service-type
            oci-service-type
            oci-service-accounts
            oci-service-profile
            oci-service-subids
            oci-configuration->shepherd-services))

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

(define (oci-runtime-requirement runtime)
  (if (eq? 'podman runtime)
      '(cgroups2-fs-owner cgroups2-limits
        rootless-podman-shared-root-fs)
      '(dockerd)))

(define (oci-runtime-name runtime)
  (if (eq? 'podman runtime)
      "Podman" "Docker"))

(define (oci-runtime-group runtime maybe-group)
  (if (maybe-value-set? maybe-group)
      maybe-group
      (if (eq? 'podman runtime)
          "cgroup"
          "docker")))

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

(define oci-image-reference
  (@@ (gnu services docker) oci-image-reference))

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
to the runtime invokation."
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
to the runtime invokation."
   (sanitizer oci-sanitize-extra-arguments)))

(define (list-of-oci-networks? value)
  (list-of-oci-records? "networks" oci-network-configuration? value))

(define-configuration/no-serialization oci-configuration
  (runtime
   (symbol 'docker)
   "The OCI runtime to use to run commands."
   (sanitizer oci-sanitize-runtime))
  (runtime-cli
   (maybe-package)
   "The OCI runtime command line to be installed in the system profile and used
to provision OCI resources.  When unset it will default to @code{docker-cli}
package for the @code{'docker} runtime or to @code{podman} package for the
@code{'podman} runtime.")
  (user
   (string "oci-container")
   "The user under whose authority OCI runtime commands will be run.")
  (group
   (maybe-string)
   "The group under whose authority OCI commands will be run.  Its default value
is either @code{docker} or @code{cgroups} based on the selected OCI runtime.")
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
containers to provision.")
  (networks
   (list-of-oci-networks '())
   "The list of @code{oci-network-configuration} records representing the
networks to provision.")
  (volumes
   (list-of-oci-volumes '())
   "The list of @code{oci-volume-configuration} records representing the
volumes to provision.")
  (verbose?
   (boolean #f)
   "When true, additional output will be printed, allowing to better follow the
flow of execution."))

(define (oci-runtime-cli config)
  (let ((runtime-cli
         (oci-configuration-runtime-cli config))
        (runtime
         (oci-configuration-runtime config)))
    #~(string-append
       (if #$(maybe-value-set? runtime-cli)
           #$runtime-cli
           "/run/current-system/profile")
       (if #$(eq? 'podman runtime)
           "/bin/podman"
           "/bin/docker"))))

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
  (basename
   (if (string? image)
       (first (string-split image #\:))
       (mainline:oci-image-repository image))))

(define (oci-object-command-shepherd-action object-name invokation)
  (shepherd-action
   (name 'command-line)
   (documentation
    (format #f "Prints ~a's OCI runtime command line invokation."
            object-name))
   (procedure
    #~(lambda _
        (format #t "~a~%" #$invokation)))))

(define (oci-container-shepherd-name runtime config)
  (define name (mainline:oci-container-configuration-provision config))
  (define image (mainline:oci-container-configuration-image config))

  (if (maybe-value-set? name)
      name
      (string-append (symbol->string runtime) "-"
                     (oci-image->container-name image))))

(define (oci-network-shepherd-name runtime)
  (string-append (symbol->string runtime) "-networks"))

(define (oci-volume-shepherd-name runtime)
  (string-append (symbol->string runtime) "-volumes"))

(define oci-container-configuration->options
  (lambda (config)
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
                         (mainline:oci-container-configuration-volumes config))))))))

(define (oci-network-configuration->options config)
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
  (append-map
   (lambda (spec)
     (list "--label" spec))
   (oci-volume-configuration-labels config)))

(define lower-oci-image
  (@@ (gnu services docker) lower-oci-image))

(define* (oci-image-loader runtime-cli name image tag #:key (verbose? #f))
  (let ((tarball (lower-oci-image name image)))
    (with-imported-modules '((guix build utils))
      (program-file (format #f "~a-image-loader" name)
       #~(begin
           (use-modules (guix build utils)
                        (ice-9 popen)
                        (ice-9 rdelim))

           (format #t "Loading image for ~a from ~a...~%" #$name #$tarball)
           (define load-command
             (string-append #$runtime-cli
                            " load -i " #$tarball))
           (when #$verbose?
             (format #t "Running ~a~%" load-command))
           (define line
             (read-line
              (open-input-pipe load-command)))

           (unless (or (eof-object? line)
                       (string-null? line))
             (format #t "~a~%" line)
             (let* ((repository&tag
                     (string-drop line
                                  (string-length
                                   "Loaded image: ")))
                    (tag-command
                     (list #$runtime-cli "tag" repository&tag #$tag)))

               (when #$verbose?
                 (format #t "Running~{ ~a~}~%" tag-command))

               (apply invoke tag-command)
               (format #t "Tagged ~a with ~a...~%" #$tarball #$tag))))))))

(define* (oci-container-entrypoint runtime-cli name image image-reference
                                   invokation #:key (verbose? #f))
  (program-file
   (string-append "oci-entrypoint-" name)
   #~(begin
       (use-modules (ice-9 format))
       (define invokation (list #$@invokation))
       #$@(if (mainline:oci-image? image)
              #~((system*
                  #$(oci-image-loader
                     runtime-cli name image
                     image-reference #:verbose? verbose?)))
              #~())
       (apply execlp invokation))))

(define* (oci-container-shepherd-service runtime runtime-cli config
                                         #:key
                                         (oci-requirement '())
                                         (user #f)
                                         (group #f)
                                         (verbose? #f))
  (let* ((actions (mainline:oci-container-configuration-shepherd-actions config))
         (auto-start?
          (mainline:oci-container-configuration-auto-start? config))
         (user (or user (mainline:oci-container-configuration-user config)))
         (group (if (and group (maybe-value-set? group))
                    group
                    (mainline:oci-container-configuration-group config)))
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
          ;; run [OPTIONS] IMAGE [COMMAND] [ARG...]
          `(,runtime-cli ,runtime-cli "run"
            "--rm" "--name" ,name
            ,@options ,@extra-arguments
            ,image-reference ,@command)))

    (shepherd-service (provision `(,(string->symbol name)))
                      (requirement `(,@(oci-runtime-requirement runtime)
                                     user-processes
                                     ,@oci-requirement
                                     ,@requirement))
                      (respawn? respawn?)
                      (auto-start? auto-start?)
                      (documentation
                       (string-append
                        (oci-runtime-name runtime) " backed Shepherd service for "
                        (if (mainline:oci-image? image) name image) "."))
                      (start
                       #~(lambda _
                           (fork+exec-command
                            (list
                             #$(oci-container-entrypoint
                                runtime-cli name image image-reference
                                invokation #:verbose? verbose?))
                            #:user #$user
                            #:group #$group
                            #$@(if (maybe-value-set? log-file)
                                   (list #:log-file log-file)
                                   '())
                            #:environment-variables
                            (append
                             (list #$@host-environment)
                             (if (eq? #$runtime 'podman)
                                 (list
                                  (string-append
                                   "HOME=" (passwd:dir (getpwnam #$user))))
                                 '())))))
                      (stop
                       #~(lambda _
                           (invoke #$runtime-cli "rm" "-f" #$name)))
                      (actions
                       (append
                        (list
                         (oci-object-command-shepherd-action
                          name #~(string-join (cdr (list #$@invokation)) " ")))
                        (if (mainline:oci-image? image)
                            '()
                            (list
                             (shepherd-action
                              (name 'pull)
                              (documentation
                               (format #f "Pull ~a's image (~a)."
                                       name image))
                              (procedure
                               #~(lambda _
                                   (invoke #$runtime-cli "pull" #$image))))))
                        actions)))))

(define (oci-object-create-invokation object runtime-cli name options
                                      extra-arguments)
  ;; network|volume create [options] [NAME]
  #~(list #$runtime-cli #$object "create"
          #$@options #$@extra-arguments #$name))

(define (format-oci-invokations invokations)
  #~(string-join (map (lambda (i) (string-join i " "))
                      (list #$@invokations))
                 "\n"))

(define* (oci-object-create-script object runtime runtime-cli invokations
                                   #:key (verbose? #f))
  (define runtime-string (symbol->string runtime))
  (program-file
   (string-append runtime-string "-" object "s-create.scm")
   #~(begin
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

       (define (object-exists? name)
         (if (string=? #$runtime-string "podman")
             (let ((command
                    (list #$runtime-cli
                          #$object "exists" name)))
               (when #$verbose?
                 (format #t "Running~{ ~a~}~%" command))
               (define exit-code (status:exit-val (apply system* command)))
               (when #$verbose?
                 (format #t "Exit code: ~a~%" exit-code))
               (equal? EXIT_SUCCESS exit-code))
             (let ((command
                    (string-append #$runtime-cli
                                   " " #$object " ls --format "
                                   "\"{{.Name}}\"")))
               (when #$verbose?
                 (format #t "Running ~a~%" command))
               (member name (read-lines (open-input-pipe command))))))

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

(define* (oci-network-shepherd-service config
                                       #:key (user #f)
                                             (group #f)
                                             (verbose? #f))
  (let* ((runtime (oci-configuration-runtime config))
         (runtime-cli
          (oci-runtime-cli config))
         (requirement
          (oci-runtime-requirement runtime))
         (networks
          (oci-configuration-networks config))
         (name (oci-network-shepherd-name runtime))
         (invokations
          (map
           (lambda (network)
             (oci-object-create-invokation
              "network" runtime-cli
              (oci-network-configuration-name network)
              (oci-network-configuration->options network)
              (oci-network-configuration-extra-arguments network)))
           networks)))

    (shepherd-service (provision `(,(string->symbol name)))
                      (requirement `(user-processes networking ,@requirement))
                      (one-shot? #t)
                      (documentation
                       (string-append
                        (oci-runtime-name runtime)
                        " network provisioning service"))
                      (start
                       #~(lambda _
                           (fork+exec-command
                            (list
                             #$(oci-object-create-script
                                "network" runtime runtime-cli
                                invokations
                                #:verbose? verbose?))
                            #:user #$user
                            #:group #$group
                            #$@(if (eq? runtime 'podman)
                                   (list
                                    #:environment-variables
                                    #~(string-append
                                       "HOME=" (passwd:dir (getpwnam #$user))))
                                   '()))))
                      (actions
                       (list
                        (oci-object-command-shepherd-action
                         name (format-oci-invokations invokations)))))))

(define* (oci-volume-shepherd-service config #:key (user #f) (group #f) (verbose? #f))
  (let* ((runtime (oci-configuration-runtime config))
         (runtime-cli
          (oci-runtime-cli config))
         (requirement
          (oci-runtime-requirement runtime))
         (volumes
          (oci-configuration-volumes config))
         (name (oci-volume-shepherd-name runtime))
         (invokations
          (map
           (lambda (volume)
             (oci-object-create-invokation
              "volume" runtime-cli
              (oci-volume-configuration-name volume)
              (oci-volume-configuration->options volume)
              (oci-volume-configuration-extra-arguments volume)))
           volumes)))

    (shepherd-service (provision `(,(string->symbol name)))
                      (requirement `(user-processes ,@requirement))
                      (one-shot? #t)
                      (documentation
                       (string-append
                        (oci-runtime-name runtime)
                        " volume provisioning service"))
                      (start
                       #~(lambda _
                           (fork+exec-command
                            (list
                             #$(oci-object-create-script
                                "volume" runtime runtime-cli
                                invokations
                                #:verbose? verbose?))
                            #:user #$user
                            #:group #$group
                            #$@(if (eq? runtime 'podman)
                                   (list
                                    #:environment-variables
                                    #~(string-append
                                       "HOME=" (passwd:dir (getpwnam #$user))))
                                   '()))))
                      (actions
                       (list
                        (oci-object-command-shepherd-action
                         name (format-oci-invokations invokations)))))))

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

(define (oci-configuration->shepherd-services config)
  (let* ((runtime (oci-configuration-runtime config))
         (runtime-cli
          (oci-runtime-cli config))
         (networks?
          (> (length (oci-configuration-networks config)) 0))
         (networks-requirement
          (if networks?
              (list
               (string->symbol
                (oci-network-shepherd-name runtime)))
              '()))
         (volumes?
          (> (length (oci-configuration-volumes config)) 0))
         (volumes-requirement
          (if volumes?
              (list
               (string->symbol
                (oci-volume-shepherd-name runtime)))
              '()))
         (user (oci-configuration-user config))
         (maybe-group (oci-configuration-group config))
         (group (oci-runtime-group runtime maybe-group))
         (verbose? (oci-configuration-verbose? config)))
    (append
     (map
      (lambda (c)
        (oci-container-shepherd-service
         runtime runtime-cli c
         #:user user
         #:group group
         #:oci-requirement
         (append networks-requirement volumes-requirement)
         #:verbose? verbose?))
      (oci-configuration-containers config))
     (if networks?
         (list
          (oci-network-shepherd-service config #:user user #:group group
                                        #:verbose? verbose?))
         '())
     (if volumes?
         (list
          (oci-volume-shepherd-service config #:user user #:group group
                                       #:verbose? verbose?))
         '()))))

(define (oci-service-subids config)
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
      (if (maybe-value-set? subgids)
          subgids
          (subid-range (name user)))
      container-users)))
  (define subuid-ranges
    (delete-duplicate-ranges
     (cons
      (if (maybe-value-set? subuids)
          subuids
          (subid-range (name user)))
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
              oci-network-shepherd-name))
   (volumes (oci-objects-merge-lst
             (oci-extension-volumes a)
             (oci-extension-volumes b)
             "volume"
             oci-volume-shepherd-name))))

(define (oci-service-profile config)
  (let ((runtime-cli
         (oci-configuration-runtime-cli config))
        (runtime
         (oci-configuration-runtime config)))
    (list bash-minimal
          (cond
           ((maybe-value-set? runtime-cli)
            runtime-cli)
           ((eq? 'podman runtime)
            podman)
           (else
            docker-cli)))))

(define oci-service-type
  (service-type (name 'oci)
                (extensions
                 (list
                  (service-extension profile-service-type
                                     oci-service-profile)
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
                (extend
                 (lambda (config extension)
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
                               oci-network-shepherd-name))
                    (volumes (oci-objects-merge-lst
                              (oci-configuration-volumes config)
                              (oci-extension-volumes extension)
                              "volume"
                              oci-volume-shepherd-name)))))
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
