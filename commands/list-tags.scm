(define-command 'list-tags
  #<#EOF
list-tags
  List tags.
EOF
  (lambda (args)
    (for-each print (db-list-tags))))
