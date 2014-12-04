(module vault-lib

(initialize-home
 load-config
 )

(import chicken scheme)
(use data-structures extras files posix utils)
(use vault-config vault-utils vault-db)

;;; Constants
(define config-file
  (make-pathname (get-environment-variable "HOME") ".vault.conf"))


;;; Initialization
(define (initialize-home)
  (unless (file-exists? (db-file))
    (create-directory (vault-home) 'recusively)
    (initialize-database (db-file)))
  (create-directory (img-download-dir) 'recursively)
  (create-directory (doc-download-dir) 'recursively))

(define (load-config)
  (cond ((file-read-access? config-file)
         (debug 2 "Loading user configuration file ~a" config-file)
         (load config-file))
        (else
         (debug 2 "User config file doesn't exist or is not readable. Skipping."))))

) ;; end module
