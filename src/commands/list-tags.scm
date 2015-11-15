(define help-list-tags
  #<#EOF
list-tags
  List tags.
EOF
)

(define (cmd-list-tags)
  (for-each (lambda (tag)
              (print (car tag)))
            (db-list-tags)))
