(module vault-cmd ()

(import chicken scheme)
(use data-structures extras files posix srfi-1 srfi-13 utils)
(use vault-utils vault-lib vault-db)

(initialize-home)
(load-config)


;;; Initial line parsing

(let* ((args (command-line-arguments)))
  (when (null? args)
    (usage 1))
  (let ((cmd (string->symbol (car args))))
    (case cmd
      ((note) (cmd-note (cdr args)))
      ((dump) (cmd-dump))
      ;((uri) (cmd-uri (cdr args)))
      (else (die! "Invalid command: ~a" cmd)))))

) ;; end module
