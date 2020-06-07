(define-command 'del
  #<#EOF
del <id>
  Remove vault objects by id.  Multiple <id>s may be provided.
EOF
  (lambda (args)
    (when (and (not (null? args))
               (help-option? (car args)))
      (command-usage 'del 0))
    (for-each
     (lambda (id)
       (let ((obj (db-get-object-by-id id)))
         (when obj
           (let ((files (vault-obj-files obj)))
             (for-each
              (lambda (file)
                (let ((obj-ids-owning-file (db-get-object-ids-linked-to-file file)))
                  ;; obj-ids-owning-file contains at least `id'
                  (if (null? (cdr obj-ids-owning-file))
                      (begin
                        (info "Deleting file ~a linked to object ~a" file id)
                        (delete-file* (make-pathname (download-dir) file)))
                      (info "Keeping file ~a as it is linked to the following objects: ~a"
                            file
                            (string-intersperse
                             (map number->string (delete id obj-ids-owning-file))
                             ", ")))))
              files)
             (db-delete-object-by-id id)))))
     (filter-map string->number args))))
