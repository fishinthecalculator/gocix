(use-modules (blue subprocess)
             (blue types blueprint)
             (blue types command))

(define-command (check-command arguments)
  ((invoke "check")
   (category 'dispatch))
  (zero?
   (popen "guix"
          `("build"
            "-L" ,(in-vicinity (getcwd) "modules")
            "-m" "etc/manifests/system-tests.scm"))))

(define-command (check-locked-command arguments)
  ((invoke "check-locked")
   (category 'dispatch))
  (zero?
   (popen "guix"
          `("time-machine" "-C" ,(in-vicinity (getcwd) "etc/channels-lock.scm")
            "--" "build" "-L" ,(in-vicinity (getcwd) "modules")
            "-m" "etc/manifests/system-tests.scm"))))

;; Refresh channels lockfile
(define-command (lock-command arguments)
  ((invoke "lock")
   (category 'dispatch))
  (and
   (zero?
    (popen "guix"
          `("time-machine" "-C" ,(in-vicinity (getcwd) "etc/channels.scm")
            "--" "describe")))
   (zero?
    (with-output-to-file (in-vicinity (getcwd) "etc/channels-lock.scm")
      (lambda _
        (popen "guix"
               `("time-machine"
                 "-C" ,(in-vicinity (getcwd) "etc/channels.scm")
                 "--" "describe" "--format=channels")))))))


(blueprint
 (commands
  (list
   check-command
   check-locked-command
   lock-command)))
