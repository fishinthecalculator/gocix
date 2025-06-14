;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (oci build utils)
  #:use-module (srfi srfi-1)
  #:export (secrets-volume-mappings))

(define* (secrets-volume-mappings secret-files #:key (mode "ro"))
  (delete-duplicates
   (map (lambda (secret-file)
          (define secret-directory (dirname secret-file))
          (string-append secret-directory ":"
                         secret-directory ":" mode))
        secret-files)))
