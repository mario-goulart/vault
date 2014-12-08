(module vault-config

(vault-home
 db-file
 download-dir
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

(define debug-level
  (make-parameter 0))

(define vault-editor
  (or (get-environment-variable "VAULT_EDITOR")
      "emacs"))

) ;; end module
