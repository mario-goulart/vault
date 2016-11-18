;;; note
(define help-note
  #<#EOF
note <summary> [-c <comment>] [-t <tag>]
  Add note with <summary> as summary. -c can be used to specify a
  comment and -t can be used to specify tags (may be used multiple times).
EOF
)

(define (cmd-note args)
  (when (null? args)
    (usage 1))
  (let ((summary (car args))
        (tags '())
        (comment #f))
    (let loop ((args (cdr args)))
      (unless (null? args)
        (let ((arg (car args)))
          (cond ((string=? arg "-c")
                 (set! comment (parse-comment-option args comment))
                 (loop (cddr args)))
                ((string=? arg "-t")
                 (set! tags (cons (parse-tag-option args) tags))
                 (loop (cddr args)))
                (else (die! "note: invalid syntax"))))))
    (db-insert-object summary (or comment 'null) tags '() '())))
