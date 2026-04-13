;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Giacomo Leidi <therewasa@fishinthecalculator.me>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (oci oci-download)
  #:use-module (oci build oci)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix store)
  #:export (oci-reference
            oci-reference?
            oci-reference-digest
            oci-reference-name
            oci-reference-tag

            skopeo-fetch
            skopeo-file-name))

;;; Commentary:
;;;
;;; An <origin> method that fetches images from OCI registries.
;;; The image is specified with an <oci-reference> object.
;;;
;;; Code:

(define-record-type* <oci-reference>
  oci-reference make-oci-reference
  oci-reference?
  (name        oci-reference-name)
  (tag         oci-reference-tag)
  (digest      oci-reference-digest))

(define (skopeo-package)
  "Return the default skopeo package."
  (let ((module (resolve-interface '(gnu packages virtualization))))
    (module-ref module 'skopeo)))

(define* (skopeo-fetch ref hash-algo hash
                       #:optional name
                       #:key (system (%current-system)) (guile (default-guile))
                       (skopeo (skopeo-package)))
  "Return a fixed-output derivation that fetches an OCI image from URL.  The
output is expected to HASH of type HASH-ALGO (a symbol).  Use NAME as the file
name, or a generic name if #f."
  (define modules
    (source-module-closure '((guix build utils))))

  (define (build)
    (define name (oci-reference-name ref))
    (define tag (oci-reference-tag ref))
    (define digest (oci-reference-digest ref))
    (define os (system->oci-os system))
    (define arch (system->oci-arch system))
    (with-imported-modules modules
      #~(begin
          (use-modules (guix build utils))
          (define source (string-append "docker://" #$name "@" #$digest))
          (define destination
           (string-append
            ;; skopeo does support also oci-archive:, which seems more
            ;; broadly compatible. docker-archive: is used in its stead,
            ;; because oci-archive: tarball hashes change every time and
            ;; seem not to be pulled reproducibly.
            "docker-archive:" #$output ":" #$name ":" #$tag))
          (define skopeo (string-append #+skopeo "/bin/skopeo"))

          (invoke skopeo "--insecure-policy" "--tmpdir" (getcwd)
                  "--override-os" #$os "--override-arch" #$arch
                  "copy" "--preserve-digests" "--src-tls-verify=false"
                  source destination)

          #t)))

  (mlet %store-monad
      ((guile (package->derivation guile system)))
    (gexp->derivation (or name "skopeo-download") (build)
                      ;; Use environment variables and a fixed script name so
                      ;; there's only one script in store for all the
                      ;; downloads.
                      #:script-name "skopeo-download"
                      #:leaked-env-vars '("LC_ALL" "LC_MESSAGES" "LANG"
                                          "COLUMNS")
                      #:system system
                      #:local-build? #t
                      #:hash-algo hash-algo
                      #:hash hash
                      #:guile-for-build guile)))

(define (skopeo-file-name ref)
  "Return the file-name for origins using skopeo-fetch."
  ;; Sanitize for the store
  (string-map (lambda (chr)
               (if (and (char-set-contains? char-set:ascii chr)
                        (char-set-contains? char-set:graphic chr)
                        (not (memv chr '(#\. #\@ #\: #\/ #\space))))
                   chr
                   #\-))
              (string-append (oci-reference-name ref) ":"
                             (oci-reference-tag ref) "@"
                             (oci-reference-digest ref))))

;;; oci-download.scm ends here
