(module vault-config

(vault-home
 db-file
 download-dir
 debug-level
 vault-editor
 downloadable-mime-types
 web-page-mime-types)

(import chicken scheme)
(use files posix)

;; Regular srfi-39 parameters don't work well when we have
;; interdependencies among them.  Example:
;;
;; (define foo (make-parameter 'foo))
;; (define bar (make-parameter (foo)))
;; (bar) ; => foo
;; (foo 'foo2)
;; (bar) ; => foo
;;
;; For our purposes, we want bar to yield foo2 in the last line of the
;; example above.  So, we need something more dynamic, with pointer
;; semantic for bindings, which is provided by define-dynamic.
;;
;; Note that, unlike srfi-39 parameters, variables defined with
;; define-dynamic are not thread-safe.
(define-syntax define-dynamic
  (syntax-rules ()
    ((_ var value)
     (define var
       (let ((slot (lambda () value)))
         (lambda args
           (if (null? args)
               (slot)
               (set! slot (lambda () (car args))))))))))

(define-dynamic vault-home
  (let ((home (get-environment-variable "HOME"))) ;; FIXME: windows?
    (make-pathname home ".vault")))

(define-dynamic db-file
  (make-pathname (vault-home) "vault.db"))

(define-dynamic download-dir
  (make-pathname (vault-home) "downloads"))

(define downloadable-mime-types
  (make-parameter
   '(application/pdf
     application/postscript
     audio/mpeg
     image/bmp
     image/gif
     image/jpeg
     image/png
     image/tiff
     video/mpeg
     video/quicktime
     video/x-msvideo)))

(define web-page-mime-types
  (make-parameter
   '(application/xhtml+xml
     text/html)))

(define debug-level
  (make-parameter 0))

(define vault-editor
  (or (get-environment-variable "VAULT_EDITOR")
      "emacs"))

) ;; end module
