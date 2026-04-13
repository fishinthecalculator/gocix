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

(define-module (oci build oci)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (system->oci-arch
            system->oci-os))

;;; Commentary:
;;;
;;; This is the build-side support code of (guix oci-download) and (guix docker).
;;; It allows an OCI image to be pulled from a registry and stored as a tarball.
;;;
;;; Code:

(define (system->oci-os system)
  (define guix-os (second (string-split system #\-)))
  (match guix-os
    ("gnu" "hurd")
    ("linux" "linux")
    (_
     (error
      (format #f "Unknown OCI image os: ~a" guix-os)))))

(define (system->oci-arch system)
  (define guix-arch (first (string-split system #\-)))
  (match guix-arch
    ("x86_64" "amd64")
    ("aarch64" "arm64")
    ((or "arm" "armhf") "arm")
    ((or "i586" "i686") "386")
    ("mips64" "mips64le")
    ("powerpc" "ppc")
    ("powerpc64le" "ppc64le")
    ("riscv64" "riscv64")
    ("s390x" "s390x")
    (_
     (error
      (format #f "Unknown OCI image architecture: ~a" guix-arch)))))
