(define-module (oci oci-download)
  #:use-module (guix records)
  #:export (oci-reference
            oci-reference?
            oci-reference-url
            oci-reference-tag

            oci-fetch

            oci-file-name))

;;; Commentary:
;;;
;;; An <origin> method that fetches a specific tagged image from an OCI registry.
;;; The registry URL and tag are specified with an <oci-reference>
;;; object.
;;;
;;; Code:

(define-record-type* <oci-reference>
  oci-reference make-oci-reference
  oci-reference?
  (url        oci-reference-url)
  (tag     oci-reference-tag))

(define (podman-package)
  "Return the default Git package."
  (let ((distro (resolve-interface '(gnu packages containers))))
    (module-ref distro 'podman)))

(define (nss-certs-package)
  "Return the default nss-certs package."
  (let ((module (resolve-interface '(gnu packages nss))))
    (module-ref module 'nss-certs)))

(define* (oci-fetch url tag
                    #:optional name
                    #:key (system (%current-system)) (guile (default-guile))
                    (podman (podman-package))
                    (nss-certs (nss-certs-package)))
  "Return a fixed-output derivation that fetches a TAGged OCI image from URL.  The output
is expected to HASH of type HASH-ALGO (a symbol).  Use NAME as the file name,
or a generic name if #f."
  (let* ((oci-email (read-config oci-help-message 'oci 'email))
         (oci-password (read-config oci-help-message 'oci 'password)))

    (define modules
      (source-module-closure '((guix build utils))))

    (define (build ca-certificates)
      (with-imported-modules modules
        #~(begin
            (use-modules (guix build utils))

            (let ((oci-reference (string-append #+url ":" #+tag))
                  (podman (string-append #+podman
                                         "/bin/podman"))
                  (cert-file (string-append #+ca-certificates
                                            "/etc/ssl/certs/ca-certificates.crt")))
              ;; ;; locidownloader insists on writing to these directories.
              ;; (setenv "XDG_CONFIG_HOME" "config")
              ;; (setenv "XDG_CACHE_HOME" "cache")

              ;; Point podman to the certificate bundle.
              (setenv "CURL_CA_BUNDLE" cert-file)

              (invoke podman "pull" oci-reference)
              (invoke podman "save"
                      "--compress"
                      "--format" "oci-archive"
                      "--output" #$output
                      oci-reference)
              (invoke podman "rm" "-f" oci-reference)
              #t))))

    (mlet %store-monad
        ((guile (package->derivation guile system))
         (ca-certificates (ca-certificate-bundle
                           (packages->manifest (list nss-certs))
                           system)))
      (gexp->derivation (or name "oci-download") (build ca-certificates)

                        ;; Use environment variables and a fixed script name so
                        ;; there's only one script in store for all the
                        ;; downloads.
                        #:script-name "oci-download"
                        ;; #:env-vars
                        ;; `(("OCI_URL" . ,url)
                        ;;   ("OCI_EMAIL" . ,oci-email)
                        ;;   ("OCI_PASSWORD" . ,oci-password))
                        #:leaked-env-vars '("http_proxy" "https_proxy"
                                            "LC_ALL" "LC_MESSAGES" "LANG"
                                            "COLUMNS")
                        #:system system
                        #:local-build? #t
                        #:hash-algo hash-algo
                        #:hash hash
                        #:guile-for-build guile))))

(define (oci-file-name url tag)
  "Return the file-name for origins using oci-download."
  (string-append
   ;; Sanitize for the store
   (string-map (lambda (chr))
               (if (and (char-set-contains? char-set:ascii chr)
                        (char-set-contains? char-set:graphic chr)
                        (not (memv chr '(#\. #\/ #\space))))
                   chr
                   #\-)
               url)
   "-" tag "-image"))
