;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (oci services configuration)
  #:use-module (gnu services configuration)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:export (field-name->environment-variable
            serialize-environment-variable
            serialize-boolean-environment-variable

            serialize-ini-string
            serialize-ini-integer
            serialize-ini-boolean

            serialize-yaml-string
            serialize-yaml-maybe-string
            configuration->yaml-block

            format-yaml-list
            format-json-list))

;; Common

(define* (format-squared-list values #:key (quoter "\"") (delimiter ", "))
  (string-append "["
               (string-join
                (map (lambda (s) (string-append quoter s quoter))
                     values)
                delimiter)
               "]"))

;; Environment variables

(define* (field-name->environment-variable field-name #:key (prefix #f))
  (let* ((str (symbol->string field-name))
         (variable
           ;; a-field? -> ${PREFIX}A_FIELD
           (string-upcase
            (string-replace-substring
             (if (string-suffix? "?" str)
                 (string-drop-right str 1)
                 str)
             "-" "_"))))
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

;; INI

(define (ini-uglify-field-name field-name)
  (string-replace-substring
   (string-replace-substring
    (symbol->string field-name) "?" "") "-" "_"))

(define (serialize-ini-string field-name value)
  #~(string-append #$(ini-uglify-field-name field-name) " = " #$value "\n"))

(define (serialize-ini-integer field-name value)
  (serialize-ini-string field-name (number->string value)))

(define (serialize-ini-boolean field-name value)
  (serialize-ini-string field-name (if value "true" "false")))


;; Yaml

(define (yaml-uglify-field-name field-name)
  (string-replace-substring
   (string-replace-substring
    (symbol->string field-name) "?" "") "-" "_"))

(define* (serialize-yaml-string field-name value #:key (indentation "") (first? #f))
  #~(string-append #$indentation (if #$first? "- " "") #$(ini-uglify-field-name field-name) ": " #$value "\n"))

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
                                         (G_ "Unknown field type: ~a")
                                         type))))
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
