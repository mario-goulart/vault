(module vault-utils *

(import scheme)
(cond-expand
  (chicken-4
   (import chicken)
   (use data-structures extras)
   (use vault-config))
  (chicken-5
   (import (chicken base)
           (chicken format)
           (chicken string))
   (import vault-config))
  (else
   (error "Unsupported CHICKEN version.")))

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

(define (vault-obj->alist obj)
  `((id                . ,(vault-obj-id obj))
    (summary           . ,(vault-obj-summary obj))
    (comment           . ,(vault-obj-comment obj))
    (creation-time     . ,(vault-obj-creation-time obj))
    (modification-time . ,(vault-obj-modification-time obj))
    (tags              . ,(vault-obj-tags obj))
    (files             . ,(vault-obj-files obj))
    (uris              . ,(vault-obj-uris obj))
    ))

(define (alist->vault-obj alist)
  (make-vault-obj
   (or (alist-ref 'id alist)
       (error 'alist->vault-obj "No id field in ~S" alist))
   (alist-ref 'summary alist)
   (alist-ref 'comment alist)
   (or (alist-ref 'creation-time alist)
       (error 'alist->vault-obj "No creation-time in ~S" alist))
   (alist-ref 'modification-time alist)
   (or (alist-ref 'tags alist) '())
   (or (alist-ref 'files alist) '())
   (or (alist-ref 'uris alist) '())
   ))

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
