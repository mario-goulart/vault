(define help-dump
  #<#EOF
dump
  Dump the database content.
EOF
)

(define (cmd-dump)
  (for-each
   (lambda (obj)
     (let ((tags (vault-obj-tags obj))
           (comment (vault-obj-comment obj))
           (filename (vault-obj-filename obj))
           (creation-time (vault-obj-creation-time obj))
           (last-modified (vault-obj-modification-time obj)))
       (printf "[~a] ~a\n"
               (vault-obj-id obj)
               (vault-obj-summary obj))
       (unless (null? tags)
         (printf "  tags: ~S\n" tags))
       (unless (null? comment)
         (printf "  comment: ~a\n" comment))
       (unless (null? filename)
         (printf "  filename: ~a\n"
                 (make-pathname (download-dir) filename)))
       (printf "  creation time: ~a\n" creation-time)
       (unless (equal? creation-time last-modified)
         (printf "  last modified: ~a\n" last-modified))
       (newline)))
   (db-dump-objects)))
