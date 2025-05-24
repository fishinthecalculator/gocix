;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (oci build oci-containers)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:export (oci-read-lines
            oci-system*
            oci-object-exists?
            oci-object-service-available?
            oci-image-load
            oci-log-verbose
            oci-container-execlp
            oci-object-create))

(define* (oci-read-lines invocation #:key verbose?)
  (define (get-lines port)
    (let ((lines-string (get-string-all port)))
      (string-split lines-string #\newline)))

  (define command
   (string-join invocation " "))

  (when verbose? (format #t "Running ~a~%" command))

  (with-input-from-port (open-input-pipe command)
    (lambda _
      (get-lines (current-input-port)))))

(define* (oci-log-verbose invocation)
  (format #t "Running in verbose mode...
Current user: ~a ~a
Current group: ~a ~a
Current directory: ~a~%"
          (getuid) (passwd:name (getpwuid (getuid)))
          (getgid) (group:name (getgrgid (getgid)))
          (getcwd))

  (format #t "Running~{ ~a~}~%" invocation))

(define* (oci-system* invocation #:key verbose?)
  (when verbose?
    (format #t "Running~{ ~a~}~%" invocation))

  (let* ((status (apply system* invocation))
         (exit-code (status:exit-val status)))
    (when verbose?
      (format #t "Exit code: ~a~%" exit-code))
    status))

(define* (oci-object-member name objects
                            #:key verbose?)

  (define member? (member name objects))

  (when (and verbose? (> (length objects) 0))
    (format #t "~a is ~apart of:~{ ~a~}~%"
            name
            (if member? "" "not ")
            objects))
  member?)

(define* (oci-object-list runtime-cli object
                          #:key verbose?
                          (format-string "{{.Name}}"))

  (define invocation
    (list runtime-cli object "ls" "--format"
          (string-append "\"" format-string "\"")))

  (filter
   (lambda (name)
     (not (string=? (string-trim name) "")))
   (oci-read-lines invocation #:verbose? verbose?)))

(define* (docker-object-exist? runtime-cli object name
                               #:key verbose?
                               (format-string "{{.Name}}"))

  (define objects
    (oci-object-list runtime-cli object
                     #:verbose? verbose?
                     #:format-string format-string))

  (oci-object-member name objects #:verbose? verbose?))

(define* (podman-object-exist? runtime-cli object name #:key verbose?)
  (let ((invocation (list runtime-cli object "exists" name)))
    (define exit-code
      (status:exit-val (oci-system* invocation #:verbose? verbose?)))
    (equal? EXIT_SUCCESS exit-code)))

(define* (oci-object-exists? runtime runtime-cli object name
                             #:key verbose?
                             (format-string "{{.Name}}"))
  (if (eq? runtime 'podman)
      (podman-object-exist? runtime-cli object name
                            #:verbose? verbose?)
      (docker-object-exist? runtime-cli object name
                            #:verbose? verbose?
                            #:format-string format-string)))

(define* (oci-object-service-available? runtime-cli object names
                                        #:key verbose?
                                        (format-string "{{.Name}}"))
  "Whether NAMES are provisioned in the current OBJECT environment."
  (define environment
    (oci-object-list runtime-cli object
                     #:verbose? verbose?
                     #:format-string format-string))
  (when verbose?
    (format #t "~a environment:~{ ~a~}~%" object environment))

  (define available?
    (every
     (lambda (name)
       (oci-object-member name environment #:verbose? verbose?))
     names))

  (when verbose?
   (format #t "~a service is~a available~%" object (if available? "" " not")))

  available?)

(define* (oci-image-load runtime runtime-cli tarball name tag
                         #:key verbose?
                         (format-string "{{.Repository}}:{{.Tag}}"))
  (define load-invocation
    (list runtime-cli "load" "-i" tarball))

  (if (oci-object-exists? runtime runtime-cli "image" tag
                          #:verbose? verbose?
                          #:format-string format-string)
      (format #t "~a image already exists, skipping.~%" tag)
      (begin
        (format #t "Loading image for ~a from ~a...~%" name tarball)

        (let ((line (first
                     (oci-read-lines load-invocation #:verbose? verbose?))))
          (unless (or (eof-object? line)
                      (string-null? line))

            (format #t "~a~%" line)

            (let* ((repository&tag
                    (string-drop line
                                 (string-length
                                  "Loaded image: ")))
                   (tag-invocation
                    (list runtime-cli "tag" repository&tag tag))
                   (drop-old-tag-invocation
                    (list runtime-cli "image" "rm" "-f" repository&tag)))

              (unless (string=? repository&tag tag)
                (let ((exit-code
                       (status:exit-val
                        (oci-system* tag-invocation #:verbose? verbose?))))
                  (format #t "Tagged ~a with ~a...~%" tarball tag)

                  (when (equal? EXIT_SUCCESS exit-code)
                    (oci-system* drop-old-tag-invocation #:verbose? verbose?))))))))))

(define* (oci-container-execlp invocation #:key verbose? pre-script)
  (when pre-script
    (pre-script))
  (when verbose?
    (oci-log-verbose invocation))
  (apply execlp (first invocation) invocation))

(define* (oci-object-create runtime runtime-cli runtime-name
                            object
                            invocations
                            #:key verbose?
                            (format-string "{{.Name}}"))
  (for-each
   (lambda (invocation)
     (define name (last invocation))
     (if (oci-object-exists? runtime runtime-cli object name
                             #:format-string format-string
                             #:verbose? verbose?)
         (format #t "~a ~a ~a already exists, skipping creation.~%"
                 runtime-name name object)
         (oci-system* invocation #:verbose? verbose?)))
   invocations))
