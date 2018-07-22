(define-command 'search
  #<#EOF
search [-I] [-e <except regex>] <regex>
  Search for <regex> in the vault database.  -I makes search case-sensitive.
  -e can be provided multiple times and is not affected by -I.
EOF
  (lambda (args)
    (when (null? args)
      (usage 1))
    (let ((case-insensitive? #t)
          (excepts '())
          (regex '()))
      (let loop ((args args))
        (unless (null? args)
          (let ((arg (car args)))
            (cond ((string=? arg "-I")
                   (set! case-insensitive? #f)
                   (loop (cdr args)))
                  ((string=? arg "-e")
                   (when (null? (cdr args))
                     (die! "-e requires an argument."))
                   (set! excepts (cons (cadr args) excepts))
                   (loop (cddr args)))
                  (else
                   (set! regex (cons arg regex))
                   (loop (cdr args)))))))
      (when (or (null? regex)
                (not (null? (cdr regex))))
        (die! "Exactly one regex must be provided."))
      (for-each print-vault-obj
                (db-search (car regex) excepts case-insensitive?)))))
