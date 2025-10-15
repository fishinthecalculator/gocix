;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Giacomo Leidi <therewasa@fishinthecalculator.me>

(define-module (oci build utils)
  #:use-module (srfi srfi-1)
  #:export (secrets-volume-mappings))

(define* (secrets-volume-mappings secret-files #:key (mode "ro"))
  (define (secret->mapping secret)
    (string-append secret ":" secret ":" mode))
  (if (= 1 (length secret-files))
      (map secret->mapping secret-files)
      (delete-duplicates
       (map (lambda (secret-file)
              (define secret (dirname secret-file))
              (secret->mapping secret))
            secret-files))))
