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

(define vault-home
  (make-parameter
   (let ((home (get-environment-variable "HOME"))) ;; FIXME: windows?
     (make-pathname home ".vault"))))

(define db-file
  (make-parameter (make-pathname (vault-home) "vault.db")))

(define download-dir
  (make-parameter (make-pathname (vault-home) "downloads")))

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
