(define-command 'list-tags
  #<#EOF
list-tags
  List tags.
EOF
  (lambda (args)
    (for-each (lambda (tag)
                (print (car tag)))
              (db-list-tags))))
