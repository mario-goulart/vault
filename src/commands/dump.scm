(define help-dump
  #<#EOF
dump
  Dump the database content.
EOF
)

(define (cmd-dump)
  (for-each print-vault-obj (db-dump-objects)))
