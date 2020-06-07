(define-command 'list-tags
  #<#EOF
list-tags
  List tags.
EOF
  (lambda (args)
    (when (and (not (null? args))
               (help-option? (car args)))
      (command-usage 'list-tags 0))
    (for-each print (db-list-tags))))
