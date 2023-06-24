;;; note
(define-command 'note
  #<#EOF
note <summary> [-c <comment>] [-t <tag>]
  Add note with <summary> as summary. -c can be used to specify a
  comment and -t can be used to specify tags (may be used multiple times).
EOF
  (lambda (args)
    (when (null? args)
      (show-command-help 'note 1))
    (let ((summary (car args))
          (tags '())
          (comment #f))
      (let loop ((args (cdr args)))
        (unless (null? args)
          (let ((arg (car args)))
            (cond ((string=? arg "-c")
                   (set! comment (parse-option/arg args "-c" comment))
                   (loop (cddr args)))
                  ((string=? arg "-t")
                   (set! tags (cons (parse-option/arg args "-t") tags))
                   (loop (cddr args)))
                  (else (die! "note: invalid syntax"))))))
      (let ((obj-id
             (db-insert-object summary comment tags '() '())))
        (print-vault-obj (db-get-object-by-id obj-id))))))
