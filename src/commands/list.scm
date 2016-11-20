(define help-list
  #<#EOF
list [-s] [-t <tag>] [-T <tag>] [-i <id>]
  List the database content.
  -s: list in sexp format.
  -t: list entries that contain <tag>.  May be provided multiple times.
  -T: do not list entries that contain <tag>.  May be provided multiple times.
  -i: list <id>, ignoring tag filters.  May be provided multiple times.
EOF
)

(define (cmd-list args)
  (let ((list-tags '())
        (omit-tags '())
        (ids '())
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
                ((string=? arg "-i")
                 (set! ids (cons (parse-option/arg args "-i") ids))
                 (loop (cddr args)))
                ((string=? arg "-s")
                 (set! sexp-format? #t)
                 (loop (cdr args)))
                (else (die! "list: invalid syntax."))))))
    (let ((objs (if (null? ids)
                    (remove (lambda (obj)
                              (any (lambda (tag)
                                     (member tag omit-tags))
                                   (vault-obj-tags obj)))
                            (if (null? list-tags)
                                (db-dump-objects)
                                (filter (lambda (obj)
                                          (any (lambda (tag)
                                                 (member tag list-tags))
                                               (vault-obj-tags obj)))
                                        (db-dump-objects))))
                    (filter-map db-get-vault-object-by-id
                                (sort (filter-map string->number ids) <)))))
      (with-output-to-pager
       (lambda ()
         (let loop ((objs objs))
           (unless (null? objs)
             (let ((obj (car objs)))
               (if sexp-format?
                   (pp (vault-obj->alist obj))
                   (begin
                     (print-vault-obj obj)
                     (unless (null? (cdr objs))
                       (newline)))))
             (loop (cdr objs)))))))))
