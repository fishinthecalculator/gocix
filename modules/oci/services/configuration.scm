;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (oci services configuration)
  #:use-module (gnu services configuration)
  #:use-module (ice-9 string-fun)
  #:export (field-name->environment-variable
            serialize-environment-variable
            serialize-boolean-environment-variable

            format-json-list))

;; Turn field names, which are Scheme symbols into strings
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
  (string-append "["
               (string-join
                (map (lambda (s) (string-append "\"" s "\""))
                     values)
                ", ")
               "]"))
