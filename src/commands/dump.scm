(define help-dump
  #<#EOF
dump [-s]
  Dump the database content.  If -s is provided, dump in sexp format.
EOF
)

(define (cmd-dump args)
  (let ((sexp-format? (and (not (null? args))
                           (string=? (car args) "-s"))))
    (for-each
     (lambda (obj)
       (if sexp-format?
           (pp (vault-obj->alist obj))
           (print-vault-obj obj)))
     (db-dump-objects))))
