(module vault-utils *

(import chicken scheme)
(use data-structures extras)
(use vault-config)

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

 
) ;; end module