(module vault-config

(vault-home
 db-file
 download-dir
 img-download-dir
 doc-download-dir
 download-img?
 download-doc?
 img-content-types
 doc-content-types
 debug-level
 vault-editor)

(import chicken scheme)
(use files posix)

(define vault-home
  (make-parameter
   (let ((home (get-environment-variable "HOME")))
     (make-pathname home ".vault"))))

(define db-file
  (make-parameter (make-pathname (vault-home) "vault.db")))

(define download-dir
  (make-parameter (make-pathname (vault-home) "downloads")))

(define img-download-dir
  (make-parameter (make-pathname (download-dir) "img")))

(define doc-download-dir
  (make-parameter (make-pathname (download-dir) "doc")))

(define download-img?
  (make-parameter #t))

(define download-doc?
  (make-parameter #t))

(define img-content-types
  (make-parameter '())) ;; FIXME

(define doc-content-types
  (make-parameter '())) ;; FIXME

(define debug-level
  (make-parameter 0))

(define vault-editor
  (or (get-environment-variable "VAULT_EDITOR")
      "emacs"))

) ;; end module
