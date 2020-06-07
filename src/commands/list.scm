(define-command 'list
  #<#EOF
list [-s] [-t <tag>] [-T <tag>] [-i <id>]
  List the database content.
  -s: list in sexp format.
  -t: list entries that contain <tag>.  May be provided multiple times.
  -T: do not list entries that contain <tag>.  May be provided multiple times.
  -i: list <id>, ignoring tag filters.  May be provided multiple times.
EOF
  (lambda (args)
    (let ((list-tags '())
          (omit-tags '())
          (ids '())
          (sexp-format? #f))
      (let loop ((args args))
        (unless (null? args)
          (let ((arg (car args)))
            (cond ((help-option? arg)
                   (command-usage 'list 0))
                  ((string=? arg "-t")
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
      (let* ((all-objs (db))
             (objs (if (null? ids)
                      (remove (lambda (obj)
                                (any (lambda (tag)
                                       (member tag omit-tags))
                                     (vault-obj-tags obj)))
                              (if (null? list-tags)
                                  (map cdr all-objs)
                                  (filter (lambda (obj)
                                            (let ((all-tags (vault-obj-tags obj)))
                                              (every (lambda (tag)
                                                       (member tag all-tags))
                                                     list-tags)))
                                          (map cdr all-objs))))
                      (filter-map
                       (lambda (id)
                         (alist-ref id all-objs))
                       (filter-map string->number ids)))))
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
               (loop (cdr objs))))))))))
