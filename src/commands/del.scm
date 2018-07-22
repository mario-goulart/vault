(define-command 'del
  #<#EOF
del <id>
  Remove vault objects by id.  Multiple <id>s may be provided.
EOF
  (lambda (args)
    (for-each
     (lambda (id)
       (let ((obj (db-get-vault-object-by-id id)))
         (when obj
           (let ((files (vault-obj-files obj)))
             (for-each
              (lambda (file)
                (let ((objs-owning-file (db-get-obj-ids-linked-to-file file)))
                  (if (null? (cdr objs-owning-file))
                      (begin
                        (info "Deleting file ~a linked to object ~a" file id)
                        (delete-file* (make-pathname (download-dir) file)))
                      (info "Keeping file ~a as it is linked to the following objects: ~a"
                            file
                            (string-intersperse
                             (map number->string (delete id objs-owning-file))
                             ", ")))))
              files)
             (db-delete-object-by-id id)))))
     (filter-map string->number args))))
