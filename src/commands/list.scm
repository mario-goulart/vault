(define help-list
  #<#EOF
list [-s] [-t <tag>] [-T <tag>]
  List the database content.
  -s: list in sexp format.
  -t: list entries that contain <tag>.  May be provided multiple times.
  -T: do not list entries that contain <tag>.  May be provided multiple times.
EOF
)

(define (cmd-list args)
  (let ((list-tags '())
        (omit-tags '())
        (sexp-format? #f))
    (let loop ((args args))
      (unless (null? args)
        (let ((arg (car args)))
          (cond ((string=? arg "-t")
                 (set! list-tags (cons (parse-option/arg args "-t") list-tags))
                 (loop (cddr args)))
                ((string=? arg "-T")
                 (set! omit-tags (cons (parse-option/arg args "-T") omit-tags))
                 (loop (cddr args)))
                ((string=? arg "-s")
                 (set! sexp-format? #t)
                 (loop (cdr args)))
                (else (die! "list: invalid syntax."))))))
    (let ((objs (remove (lambda (obj)
                          (any (lambda (tag)
                                 (member tag omit-tags))
                               (vault-obj-tags obj)))
                        (if (null? list-tags)
                            (db-dump-objects)
                            (filter (lambda (obj)
                                      (any (lambda (tag)
                                             (member tag list-tags))
                                           (vault-obj-tags obj)))
                                    (db-dump-objects))))))
      (with-output-to-pager
       (lambda ()
         (for-each
          (lambda (obj)
            (if sexp-format?
                (pp (vault-obj->alist obj))
                (print-vault-obj obj)))
          objs))))))
