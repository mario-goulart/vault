(define-command 'attach
  #<#EOF
attach <id> <file-1> [... <file-n>]
  Attach files to vault objects identified by <id>.  Multiple files
  may be provided.
EOF
  (lambda (args)
    (when (or (null? args)
              (null? (cdr args)))
      (show-command-help 'attach 1))
    (let* ((id (string->number (car args)))
           (new-files (cdr args))
           (obj (db-get-object-by-id id)))
      (unless id
        (die! "Invalid <id>: ~a" (car args)))
      (if obj
          (let ((obj-files (vault-obj-files obj))
                (attached-files (map (lambda (file)
                                       (pathname-strip-download-dir
                                        (save-file file #t)))
                                     new-files)))
            (vault-obj-files-set! obj (append obj-files attached-files))
            (db-update-object-by-id id obj))
          (die! "attach: no such object: ~a" id)))))
