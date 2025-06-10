;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024, 2025 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (oci services configuration)
  #:use-module (gnu services configuration)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:export (positive?

            field-name->environment-variable
            serialize-string-environment-variable
            serialize-boolean-environment-variable
            configuration->environment-variables

            serialize-ini-string
            serialize-ini-integer
            serialize-ini-boolean

            serialize-yaml-string
            serialize-yaml-maybe-string
            configuration->yaml-block

            format-yaml-list
            format-json-list

            serialize-hjson-string
            configuration->hjson-block))

(define (positive? value)
  (and (integer? value) (> value 0)))

;; Common

(define* (format-squared-list values #:key (quoter "\"") (delimiter ", "))
  "Serializes VALUES, a list of strings, into a structured format where the
resulting serialization starts with @code{[} and ends with @code{]}.  DELIMITER
is interleaved between elements and each element is wrapped with QUOTER.

This procedure can be used, for example, to serialize list of strings to JSON
or YAML lists."
  (string-append "["
               (string-join
                (map (lambda (s) (string-append quoter s quoter))
                     values)
                delimiter)
               "]"))

(define (uglify-snake-case field-name)
  "Serializes FIELD-NAME, a field name from @code{(gnu services configuration)},
to a snake case string representation of the field name.  Trailing @code{?} in
the name are dropped and @code{-} get replaced by @code{_}.

For example the procedure would convert @code{'A-Field?} to @code{\"a_field\"}."
  (define str (symbol->string field-name))
  (string-downcase
   (string-replace-substring
    (if (string-suffix? "?" str)
        (string-drop-right str 1)
        str)
    "-" "_")))

;; Environment variables

(define* (field-name->environment-variable field-name #:key (prefix #f))
  "Serializes FIELD-NAME, a field name from @code{(gnu services configuration)},
to a snake case upper case string representation of the field name.  Trailing
@code{?} in the name are dropped and @code{-} get replaced by @code{_}.  When
PREFIX is a string, it is prepended to the result.

For example the procedure would convert @code{'A-Field?} to @code{\"A_FIELD\"}."
  (let ((variable (string-upcase
                   (uglify-snake-case field-name))))
    (if (string? prefix)
        (string-append prefix variable)
        variable)))

(define* (serialize-string-environment-variable field-name value
                                                #:key (prefix #f))
  #~(cons #$(field-name->environment-variable field-name #:prefix prefix)
          #$value))

(define* (serialize-boolean-environment-variable field-name value
                                                 #:key (prefix #f)
                                                 (true-value "true")
                                                 (false-value "false"))
  (serialize-string-environment-variable
   field-name
   (if value true-value false-value)
   #:prefix prefix))

(define* (serialize-number-environment-variable field-name value
                                                #:key (prefix #f))
  #~(cons #$(field-name->environment-variable field-name #:prefix prefix)
          #$(number->string value)))

(define* (configuration->environment-variables config fields
                                               #:key (excluded '())
                                               (prefix #f)
                                               (true-value "true")
                                               (false-value "false"))
  "Serializes CONFIG, a configuration from @code{(gnu services configuration)},
and its FIELDS to a list of gexps.  Each gexp will evaluate to a pair
representing an environment variable.  The first element of each pair is the
variable name, the second is the value.  When PREFIX is a string it is prepended
to the variable name.  If any of FIELDS' names are a member of EXCLUDED they
won't be serialized.  TRUE-VALUE and FALSE-VALUE will be used as a
representation for respectfully @code{#t} and @code{#f}."
  ;; Filter out unset maybe-types.
  (filter (compose not null?)
          (map (lambda (f)
                 (let ((field-name (configuration-field-name f))
                       (type (configuration-field-type f))
                       (value ((configuration-field-getter f) config)))
                   (if (and (not (eq? field-name 'image))
                            (not (member field-name excluded)))
                       (match type
                         ('string
                          (serialize-string-environment-variable
                           field-name value #:prefix prefix))
                         ('maybe-string
                          (if (maybe-value-set? value)
                              (serialize-string-environment-variable
                               field-name value #:prefix prefix)
                              '()))
                         ('number
                          (serialize-number-environment-variable
                           field-name value #:prefix prefix))
                         ('maybe-number
                          (if (maybe-value-set? value)
                              (serialize-number-environment-variable
                               field-name value #:prefix prefix)
                              '()))
                         ('positive
                          (serialize-number-environment-variable
                           field-name value #:prefix prefix))
                         ('maybe-positive
                          (if (maybe-value-set? value)
                              (serialize-number-environment-variable
                               field-name value #:prefix prefix)
                              '()))
                         ('boolean
                          (serialize-boolean-environment-variable
                           field-name value #:prefix prefix
                           #:true-value true-value #:false-value false-value))
                         ('maybe-boolean
                          (if (maybe-value-set? value)
                              (serialize-boolean-environment-variable
                               field-name value #:prefix prefix
                               #:true-value true-value
                               #:false-value false-value)
                              '()))
                         (_
                          (raise
                           (formatted-message
                            (G_ "Unknown environment-variable field type: ~a")
                            type))))
                       '())))
               fields)))

;; INI

(define (serialize-ini-string field-name value)
  #~(string-append #$(uglify-snake-case field-name) " = " #$value "\n"))

(define (serialize-ini-integer field-name value)
  (serialize-ini-string field-name (number->string value)))

(define (serialize-ini-boolean field-name value)
  (serialize-ini-string field-name (if value "true" "false")))

;; Yaml

(define* (serialize-yaml-string field-name value #:key (indentation "") (first? #f))
  #~(string-append #$indentation (if #$first? "- " "") #$(uglify-snake-case field-name) ": " #$value "\n"))

(define* (serialize-yaml-maybe-string field-name value #:key (indentation "") (first? #f))
  (if (maybe-value-set? value)
      (serialize-yaml-string field-name value #:indentation indentation #:first? first?)
      ""))

(define (format-yaml-list values)
  (format-squared-list values #:quoter "'"))

(define* (configuration->yaml-block config fields #:key (sequence? #t) (indentation "") (excluded '()))
  #~(apply string-append
           (list
            #$@(filter (compose not null?)
                       (map (lambda (pair)
                              (let* ((f (car pair))
                                     (i (cdr pair))
                                     (first? (and sequence?
                                                  (= i 0)))
                                     (indentation (if (not first?)
                                                      (string-append indentation "  ")
                                                      indentation))
                                     (field-name (configuration-field-name f))
                                     (type (configuration-field-type f))
                                     (value ((configuration-field-getter f) config)))
                                (if (not (member field-name excluded))
                                    (match type
                                      ('string
                                       (serialize-yaml-string field-name value
                                                              #:indentation indentation
                                                              #:first? first?))
                                      ('maybe-string
                                       (serialize-yaml-maybe-string field-name value
                                                                    #:indentation indentation
                                                                    #:first? first?))
                                      ('list-of-strings
                                       (serialize-yaml-string field-name
                                                              (format-yaml-list value)
                                                              #:indentation indentation
                                                              #:first? first?))
                                      (_
                                       (raise
                                        (formatted-message
                                         (G_ "Unknown yaml field type: ~a ~a")
                                         field-name type))))
                                    '())))
                            (let loop ((counter 0)
                                       (acc '())
                                       (lst fields))
                              (if (null? lst)
                                  acc
                                  (loop (1+ counter)
                                        (append acc (list (cons (car lst)
                                                                counter)))
                                        (cdr lst)))))))))


;; hjson
(define* (serialize-hjson-field field-name value #:key (indentation ""))
  #~(string-append #$indentation #$(uglify-snake-case field-name) ": " #$value "\n"))

(define* (serialize-hjson-string field-name value #:key (indentation ""))
  (serialize-hjson-field field-name (string-append "\"" value "\"")
                         #:indentation indentation))

(define* (serialize-hjson-number field-name value #:key (indentation ""))
  (serialize-hjson-field field-name (number->string value)
                         #:indentation indentation))

(define* (serialize-hjson-boolean field-name value #:key (indentation ""))
  (serialize-hjson-field field-name (if value "true" "false")
                         #:indentation indentation))

(define* (serialize-hjson-maybe-string field-name value #:key (indentation ""))
  (if (maybe-value-set? value)
      (serialize-hjson-string field-name value #:indentation indentation)
      ""))

(define* (configuration->hjson-block config fields #:key (indentation "") (excluded '()) (excluded-types '()))
  #~(apply string-append
           (list
            #$@(filter (compose not null?)
                       (map (lambda (pair)
                              (let* ((f (car pair))
                                     (i (cdr pair))
                                     (field-name (configuration-field-name f))
                                     (type (configuration-field-type f))
                                     (value ((configuration-field-getter f) config)))
                                (if (not (or (member field-name excluded)
                                             (any (lambda (predicate) (predicate field-name))
                                                  excluded-types)))
                                    (match type
                                      ('string
                                       (serialize-hjson-string field-name value
                                                               #:indentation indentation))
                                      ('maybe-string
                                       (serialize-hjson-maybe-string field-name value
                                                                     #:indentation indentation))
                                      ('number
                                       (serialize-hjson-number field-name value
                                                               #:indentation indentation))
                                      ('boolean
                                       (serialize-hjson-boolean field-name value
                                                               #:indentation indentation))
                                      ('symbol
                                       (serialize-hjson-string field-name
                                                               (symbol->string value)
                                                               #:indentation indentation))
                                      (_
                                       (raise
                                        (formatted-message
                                         (G_ "Unknown hjson field type: ~a ~a")
                                         field-name type))))
                                    '())))
                            (let loop ((counter 0)
                                       (acc '())
                                       (lst fields))
                              (if (null? lst)
                                  acc
                                  (loop (1+ counter)
                                        (append acc (list (cons (car lst)
                                                                counter)))
                                        (cdr lst)))))))))
