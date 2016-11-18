(define help-del
  #<#EOF
del <id>
  Remove vault objects by id.  Multiple <id>s may be provided.
EOF
)

(define (cmd-del args)
  (apply db-delete-object-by-id args))
