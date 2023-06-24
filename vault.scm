(module vault-cmd ()

(import scheme)
(cond-expand
  (chicken-4
   (import chicken)
   (use data-structures files posix)
   (use vault-utils vault-lib vault-db))
  (chicken-5
   (import (chicken base)
           (chicken pathname)
           (chicken process-context)
           (chicken process signal))
   (import commands)
   (import vault-utils vault-lib vault-db))
  (else
   (error "Unsupported CHICKEN version.")))

;;; Initial command line parsing
(let ((args (command-line-arguments)))
  (when (null? args)
    (show-main-help 1))

  (load-config
   (or (get-environment-variable "VAULT_CONFIG")
       (make-pathname (get-environment-variable "HOME") ".vault.conf")))

  (initialize-home)

  (when (member (car args) (help-options))
    (show-main-help 0))

  ;; Ignore SIGPIPE, otherwise with-output-to-pipe would make vault exit
  ;; without displaying the prompt, in case user terminates the pager.
  (set-signal-handler! signal/pipe void)

  (let ((cmd-name (string->symbol (car args))))
    (or (and-let* ((command (alist-ref cmd-name (commands))))
          (db (db-read))
          ((command-proc command) (cdr args))
          (db-write!))
        (die! "Invalid command: ~a" cmd-name))))

) ;; end module
