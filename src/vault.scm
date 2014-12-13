(module vault-cmd ()

(import chicken scheme)
(use vault-utils vault-lib vault-db)

(initialize-home)
(load-config)

;;; Initial command line parsing
(let* ((args (command-line-arguments)))
  (when (null? args)
    (usage 1))
  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))
  (let ((cmd (string->symbol (car args))))
    (case cmd
      ((note) (cmd-note (cdr args)))
      ((dump) (cmd-dump))
      ((list-tags) (cmd-list-tags))
      ;((uri) (cmd-uri (cdr args)))
      (else (die! "Invalid command: ~a" cmd)))))

) ;; end module
