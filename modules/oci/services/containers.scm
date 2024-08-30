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
  #:use-module (gnu system shadow)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (small-guix system accounts)
  #:use-module (small-guix system shadow)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)

  #:export (oci-network-configuration
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
            oci-configuration-user
            oci-configuration-group
            oci-configuration-containers
            oci-configuration-networks
            oci-configuration-volumes

            oci-extension
            oci-extension?
            oci-extension-fields
            oci-extension-containers
            oci-extension-networks
            oci-extension-volumes

            oci-container-service-type
            oci-service-type
            oci-container-accounts
            oci-container-subids
            oci-config->shepherd-services))

(define oci-sanitize-mixed-list
  (@@ (gnu services docker) oci-sanitize-mixed-list))

(define oci-sanitize-extra-arguments
  (@@ (gnu services docker) oci-sanitize-extra-arguments))

(define lower-oci-image
  (@@ (gnu services docker) lower-oci-image))

(define oci-image-reference
  (@@ (gnu services docker) oci-image-reference))

(define oci-container-configuration->options
  (@@ (gnu services docker) oci-container-configuration->options))

;; New code for (gnu services containers)

(define %oci-supported-runtimes
  '(docker podman))

(define (oci-runtime-requirement runtime)
  (if (eq? 'podman runtime)
      '(cgroups2-fs-owner cgroups2-limits
        rootless-podman-shared-root-fs)
      '(dockerd)))

(define* (oci-runtime-cli runtime #:key (system? #t))
  (if (eq? 'podman runtime)
      (if system?
          ;; Use binaries from PATH to allow
          ;; users to override them with their
          ;; respective Guix System services.
          "podman"
          (file-append podman "/bin/podman"))
      (if system?
          "docker"
          (file-append docker-cli "/bin/docker"))))

(define (oci-runtime-name runtime)
  (if (eq? 'podman runtime)
      "Podman" "Docker"))

(define (oci-runtime-group runtime maybe-group)
  (if (maybe-value-set? maybe-group)
      (if (eq? 'podman runtime)
          "cgroup"
          "docker")))

(define (oci-sanitize-runtime value)
  (unless (member value %oci-supported-runtimes)
    (raise
     (formatted-message
      (G_ "OCI runtime must be a symbol and one of ~a,
but ~a was found") %oci-supported-runtimes value))))

(define (oci-sanitize-labels value)
  ;; Expected spec format:
  ;; '(("foo" . "bar") "foo=bar")
  (oci-sanitize-mixed-list "labels" value "="))

(define (list-of-oci-records? name predicate value)
  (map
   (lambda (el)
     (if (predicate el)
         el
         (raise
          (formatted-message
           (G_ "~a may only be strings, gexps or file-like objects
but ~a was found") name el))))
   value))

(define (list-of-oci-containers? value)
  (list-of-oci-records? "containers" mainline:oci-container-configuration? value))
(define (list-of-oci-networks? value)
  (list-of-oci-records? "networks" oci-network-configuration? value))
(define (list-of-oci-volumes? value)
  (list-of-oci-records? "volumes" oci-volume-configuration? value))

(define-maybe/no-serialization string)

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

(define-configuration/no-serialization oci-network-configuration
  (name
   (string)
   "The name of the OCI network to provision.")
  (driver
   (string "bridge")
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
   (string "default")
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

(define-configuration/no-serialization oci-configuration
  (runtime
   (symbol 'docker)
   "The OCI runtime to use to run commands."
   (sanitizer oci-sanitize-runtime))
  (user
   (string "oci-container")
   "The user under whose authority OCI runtime commands will be run.")
  (group
   (maybe-string)
   "The group under whose authority OCI commands will be run.  Its default value
is either @code{docker} or @code{cgroups} based on the selected OCI runtime.")
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
volumes to provision."))

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

(define (oci-image-loader runtime name image tag)
  (let ((tarball (lower-oci-image name image)))
    (with-imported-modules '((guix build utils))
      (program-file (format #f "~a-image-loader" name)
       #~(begin
           (use-modules (guix build utils)
                        (ice-9 popen)
                        (ice-9 rdelim))

           (format #t "Loading image for ~a from ~a...~%" #$name #$tarball)
           (define line
             (read-line
              (open-input-pipe
               (string-append #$(oci-runtime-cli runtime)
                              " load -i " #$tarball))))

           (unless (or (eof-object? line)
                       (string-null? line))
             (format #t "~a~%" line)
             (let ((repository&tag
                    (string-drop line
                                 (string-length
                                   "Loaded image: "))))

               (invoke #$(oci-runtime-cli runtime) "tag" repository&tag #$tag)
               (format #t "Tagged ~a with ~a...~%" #$tarball #$tag))))))))

(define (oci-image->container-name image)
  (basename
   (if (string? image)
       (first (string-split image #\:))
       (mainline:oci-image-repository image))))

(define (oci-container-shepherd-name runtime config)
  (define name (symbol->string
                (mainline:oci-container-configuration-provision config)))
  (define image (mainline:oci-container-configuration-image config))
  (string->symbol
   (if (maybe-value-set? name)
       name
       (string-append (symbol->string runtime) "-"
                      (oci-image->container-name image)))))

(define (oci-network-shepherd-name runtime)
 (string->symbol
  (string-append (symbol->string runtime) "-networks")))

(define (oci-volume-shepherd-name runtime)
  (string->symbol
   (string-append (symbol->string runtime) "-volumes")))

(define* (oci-container-shepherd-service runtime config #:key (user #f) (group #f))
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
         (provision (mainline:oci-container-configuration-provision config))
         (requirement (mainline:oci-container-configuration-requirement config))
         (respawn?
          (mainline:oci-container-configuration-respawn? config))
         (image (mainline:oci-container-configuration-image config))
         (image-reference (oci-image-reference image))
         (options (oci-container-configuration->options config))
         (name
          (oci-container-shepherd-name runtime config))
         (extra-arguments
          (mainline:oci-container-configuration-extra-arguments config)))

    (shepherd-service (provision `(,(string->symbol name)))
                      (requirement `(,@(oci-runtime-requirement runtime)
                                     user-processes
                                     ,(oci-network-shepherd-name runtime)
                                     ,(oci-volume-shepherd-name runtime)
                                     ,@requirement))
                      (respawn? respawn?)
                      (auto-start? auto-start?)
                      (documentation
                       (string-append
                        (oci-runtime-name runtime) " backed Shepherd service for "
                        (if (mainline:oci-image? image) name image) "."))
                      (start
                       #~(lambda ()
                           #$@(if (mainline:oci-image? image)
                                  #~((invoke #$(oci-image-loader runtime
                                                name image image-reference)))
                                  #~())
                           (fork+exec-command
                            ;; run [OPTIONS] IMAGE [COMMAND] [ARG...]
                            (list #$(oci-runtime-cli runtime) "run" "--rm" "--name" #$name
                                  #$@options #$@extra-arguments
                                  #$image-reference #$@command)
                            #:user #$user
                            #:group #$group
                            #$@(if (maybe-value-set? log-file)
                                   (list #:log-file log-file)
                                   '())
                            #:environment-variables
                            (list #$@host-environment))))
                      (stop
                       #~(lambda _
                           (invoke #$(oci-runtime-cli runtime) "rm" "-f" #$name)))
                      (actions
                       (if (mainline:oci-image? image)
                           '()
                           (append
                            (list
                             (shepherd-action
                              (name 'pull)
                              (documentation
                               (format #f "Pull ~a's image (~a)."
                                       name image))
                              (procedure
                               #~(lambda _
                                   (invoke #$(oci-runtime-cli runtime) "pull" #$image)))))
                            actions))))))

(define (oci-network-configuration->options config)
  (let ((driver (oci-network-configuration-driver config))
        (gateway
         (oci-network-configuration-driver config))
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
                   `(,(if (maybe-value-set? gateway)
                          `("--gateway" ,gateway)
                          '())
                     ,(if internal?
                          `("--internal")
                          '())
                     ,(if (maybe-value-set? ip-range)
                          `("--ip-range" ,ip-range)
                          '())
                     "--ipam-driver" ,ipam-driver
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

(define (oci-object-create-script object runtime objects-sexps)
  (define runtime-string (symbol->string runtime))
  (program-file
   (string-append runtime-string "-" object "-"
                  (oci-runtime-name runtime) "-create.scm")
   #~(begin
       (use-modules (ice-9 match)
                    (ice-9 popen)
                    (ice-9 rdelim))

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

       (define (network-exists? name)
         (if (string=? #$runtime-string "podman")
             (equal? EXIT_SUCCESS
                     (system* #$(oci-runtime-cli runtime)
                              #$object "exists" name))
             (member name
                     (read-lines
                      (open-input-pipe
                       (string-append #$(oci-runtime-cli runtime)
                                      " " object " ls --format "
                                      "\"{{.Name}}\""))))))

       (for-each
        (match-lambda
          ((name options extra-arguments)
           (if (network-exists? name)
               (display (string-append #$(oci-runtime-name runtime)
                                       " " name " " object " already exists,"
                                       " skipping creation."))
               ;; network create [options] [NAME]
               (apply system `(#$(oci-runtime-cli runtime) object "create"
                               ,@options ,@extra-arguments ,name)))))
        '#$objects-sexps))))


(define (oci-network-create-script runtime networks)
 (oci-object-create-script "network" runtime
                           (map (lambda (n) (list (oci-network-configuration-name n)
                                                  (oci-network-configuration->options n)
                                                  (oci-network-configuration-extra-arguments n)))
                                networks)))

(define (oci-volume-create-script runtime volumes)
  (oci-object-create-script "volume" runtime
                            (map (lambda (n) (list (oci-volume-configuration-name n)
                                                   (oci-volume-configuration->options n)
                                                   (oci-volume-configuration-extra-arguments n)))
                                 volumes)))

(define* (oci-network-shepherd-service config
                                       #:key (user #f)
                                             (group #f))
  (let* ((runtime (oci-configuration-runtime config))
         (requirement
          (oci-runtime-requirement runtime))
         (networks
          (oci-configuration-networks config))
         (name (oci-network-shepherd-name runtime)))

    (shepherd-service (provision `(,(string->symbol name)))
                      (requirement `(user-processes ,@requirement))
                      (one-shot? #t)
                      (documentation
                       (string-append
                        (oci-runtime-name runtime)
                        " network provisioning service"))
                      (start
                       #~((fork+exec-command
                            (list
                             #$(oci-network-create-script runtime
                                                          networks))
                            #:user #$user
                            #:group #$group))))))

(define* (oci-volume-shepherd-service config #:key (user #f) (group #f))
  (let* ((runtime (oci-configuration-runtime config))
         (requirement
          (oci-runtime-requirement runtime))
         (volumes
          (oci-configuration-volumes config))
         (name (oci-volume-shepherd-name runtime)))

    (shepherd-service (provision `(,(string->symbol name)))
                      (requirement `(user-processes ,@requirement))
                      (one-shot? #t)
                      (documentation
                       (string-append
                        (oci-runtime-name runtime)
                        " volume provisioning service"))
                      (start
                       #~((fork+exec-command
                           (list
                            #$(oci-volume-create-script runtime
                                                        volumes))
                           #:user #$user
                           #:group #$group))))))

(define (oci-service-accounts config)
  (define user (oci-configuration-user config))
  (define maybe-group (oci-configuration-group config))
  (define runtime (oci-configuration-runtime config))
  (list (user-account
         (name user)
         (comment "OCI services account")
         (group (oci-runtime-group runtime maybe-group))
         (system? #t)
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (oci-config->shepherd-services config)
  (let* ((runtime (oci-configuration-runtime config))
         (user (mainline:oci-container-configuration-user config))
         (maybe-group (mainline:oci-container-configuration-group config))
         (group (oci-runtime-group runtime maybe-group)))
    (append (map (lambda (c)
                   (oci-container-shepherd-service runtime c
                                                   #:user user
                                                   #:group group))
                 (oci-configuration-containers config))
            (list
             (oci-network-shepherd-service config #:user user #:group group)
             (oci-volume-shepherd-service config #:user user #:group group)))))

(define (oci-service-subids c)
  (define runtime
    (oci-configuration-runtime c))
  (define user
    (oci-configuration-user c))
  (define container-users
    (map (lambda (name) (subid-range (name name)))
         (append (list user)
                 (map (lambda (cc)
                        (mainline:oci-container-configuration-user cc))
                      (oci-configuration-containers c)))))
  (if (eq? 'podman runtime)
      (subids-extension
       (subgids
        container-users)
       (subuids
        container-users))
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
remove the duplicate.") object (get-name element) object))))
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
                      (symbol->string maybe-name)
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

(define oci-service-type
  (service-type (name 'oci)
                (extensions
                 (list (service-extension profile-service-type
                                          (lambda (config)
                                            (list bash-minimal
                                                  (if (eq? 'podman
                                                           (oci-configuration-runtime config))
                                                      podman
                                                      docker-cli))))
                       (service-extension subids-service-type
                                          oci-service-subids)
                       (service-extension account-service-type
                                          oci-service-accounts)
                       (service-extension shepherd-root-service-type
                                          oci-config->shepherd-services)))
                ;; Concatenate OCI object lists.
                (compose (lambda (args)
                           (fold oci-extension-merge
                                 (oci-extension)
                                 args)))
                (extend
                 (lambda (config extension)
                   (oci-configuration
                    (inherit config)
                    (containers (oci-objects-merge-lst
                                 (oci-extension-containers config)
                                 (oci-extension-containers extension)
                                 "container"
                                 (lambda (config)
                                   (define maybe-name (mainline:oci-container-configuration-provision config))
                                   (if (maybe-value-set? maybe-name)
                                       (symbol->string maybe-name)
                                       (oci-image->container-name
                                        (mainline:oci-container-configuration-image config))))))
                    (networks (oci-objects-merge-lst
                               (oci-extension-networks config)
                               (oci-extension-networks extension)
                               "network"
                               oci-network-shepherd-name))
                    (volumes (oci-objects-merge-lst
                              (oci-extension-volumes config)
                              (oci-extension-volumes extension)
                              "volume"
                              oci-volume-shepherd-name)))))
                (default-value (oci-configuration))
                (description
                 "This service implements the proviosioning of OCI object such
as containers networks and volumes.")))

(define oci-container-service-type
  (service-type (inherit mainline:oci-container-service-type)
                (extensions (list (service-extension oci-service-type
                                                     (lambda (containers)
                                                       (oci-extension
                                                        (containers containers))))))))
