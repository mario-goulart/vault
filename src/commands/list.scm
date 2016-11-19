(define help-list
  #<#EOF
list [-s]
  List the database content.  If -s is provided, list in sexp format.
EOF
)

(define (cmd-list args)
  (let ((sexp-format? (and (not (null? args))
                           (string=? (car args) "-s"))))
    (with-output-to-pager
     (lambda ()
       (for-each
        (lambda (obj)
          (if sexp-format?
              (pp (vault-obj->alist obj))
              (print-vault-obj obj)))
        (db-dump-objects))))))
