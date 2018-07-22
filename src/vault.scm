(module vault-cmd ()

(import chicken scheme)
(use data-structures files posix)
(use vault-utils vault-lib vault-db)

;;; Initial command line parsing
(let ((args (command-line-arguments)))
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

  (maybe-migrate-db!)

  ;; Ignore SIGPIPE, otherwise with-output-to-pipe would make vault exit
  ;; without displaying the prompt, in case user terminates the pager.
  (set-signal-handler! signal/pipe void)

  (let ((cmd-name (string->symbol (car args))))
    (or (and-let* ((command (alist-ref cmd-name *commands*)))
          ((command-proc command) (cdr args)))
        (die! "Invalid command: ~a" cmd-name))))

) ;; end module
