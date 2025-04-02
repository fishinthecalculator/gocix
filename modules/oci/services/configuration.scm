;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (oci services configuration)
  #:use-module (gnu services configuration)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:export (field-name->environment-variable
            serialize-environment-variable
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

;; Common

(define* (format-squared-list values #:key (quoter "\"") (delimiter ", "))
  (string-append "["
               (string-join
                (map (lambda (s) (string-append quoter s quoter))
                     values)
                delimiter)
               "]"))

(define (uglify-snake-case field-name)
  (define str (symbol->string field-name))
  (string-downcase
   (string-replace-substring
    (if (string-suffix? "?" str)
        (string-drop-right str 1)
        str)
    "-" "_")))

;; Environment variables

(define* (field-name->environment-variable field-name #:key (prefix #f))
  (let ((variable
         ;; a-field? -> ${PREFIX}A_FIELD
         (string-upcase
          (uglify-snake-case field-name))))
    (if (and prefix (string? prefix) (not (string-null? prefix)))
        (string-append prefix variable)
        variable)))

(define* (serialize-environment-variable field-name value #:key (prefix #f))
  (if (maybe-value-set? value)
      (cons (field-name->environment-variable field-name #:prefix prefix) value)
      '()))

(define* (serialize-boolean-environment-variable field-name value #:key (prefix #f) (true-value "true") (false-value "false"))
  (serialize-environment-variable field-name (if value true-value false-value) #:prefix prefix))

(define (format-json-list values)
  (format-squared-list values))

(define* (configuration->environment-variables config fields #:key (excluded '()))
  (filter (lambda (variable)
            (and (not (null? variable))
                 (not (and (string? variable)
                           (string-null? variable)))))
          (map (lambda (f)
                 (let ((field-name (configuration-field-name f))
                       (type (configuration-field-type f))
                       (value ((configuration-field-getter f) config)))
                   (if (not (eq? field-name 'image))
                       (match type
                         ('string
                          (serialize-environment-variable field-name value))
                         ('maybe-string
                          (if (maybe-value-set? value)
                              (serialize-environment-variable field-name value)
                              '()))
                         ('boolean
                          (serialize-boolean-environment-variable field-name value))
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
