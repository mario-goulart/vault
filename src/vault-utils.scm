(module vault-utils *

(import chicken scheme)
(use data-structures extras)
(use vault-config)

(define-record vault-obj
  id
  summary
  comment
  creation-time
  modification-time
  tags
  files
  uris)

(define-record-printer (vault-obj obj out)
  (fprintf out "#<vault-obj id=~a summary=~a creation_time=~a modification_time=~a tags=~s files=~S uris=~S comment=~a>"
           (vault-obj-id obj)
           (vault-obj-summary obj)
           (vault-obj-creation-time obj)
           (vault-obj-modification-time obj)
           (vault-obj-tags obj)
           (vault-obj-files obj)
           (vault-obj-uris obj)
           (vault-obj-comment obj)))

;;; Messages
(define (printer port prefix fmt args)
  (apply fprintf `(,port
                   ,(string-append prefix ": " fmt "\n")
                   ,@args)))

(define (info fmt . args)
  (printer (current-output-port) "INFO" fmt args))

(define (warn fmt . args)
  (printer (current-error-port) "WARNING" fmt args))

(define (problem fmt . args)
  (printer (current-error-port) "ERROR" fmt args))

(define (debug level fmt . args)
  (when (< level (debug-level))
    (printer (current-error-port) (conc "DEBUG " level) fmt args)))

;;; Exiting
(define (die! fmt . args)
  (apply problem (cons fmt args))
  (exit 1))


) ;; end module
