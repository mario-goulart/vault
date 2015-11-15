(module vault-cmd ()

(import chicken scheme)
(use files posix)
(use vault-utils vault-lib vault-db)

;;; Initial command line parsing
(let* ((args (command-line-arguments)))
  (when (null? args)
    (usage 1))
  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))

  (load-config
   (or (get-environment-variable "VAULT_CONFIG")
       (make-pathname (get-environment-variable "HOME") ".vault.conf")))

  (initialize-home)

  (let ((cmd (string->symbol (car args))))
    (case cmd
      ((note) (cmd-note (cdr args)))
      ((dump) (cmd-dump))
      ((list-tags) (cmd-list-tags))
      ((uri) (cmd-uri (cdr args)))
      ((del) (cmd-del (cdr args)))
      ((search) (cmd-search (cdr args)))
      (else (die! "Invalid command: ~a" cmd)))))

) ;; end module
