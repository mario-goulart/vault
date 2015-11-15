(define help-del
  #<#EOF
del
  Remove vault objects by id.
EOF
)

(define (cmd-del args)
  (apply db-delete-object-by-id args))
