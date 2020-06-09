(define (edit-file file)
  (handle-exceptions exn
    (begin
      (delete-file file)
      (exit 1))
    (system* (sprintf "~a ~a" (vault-editor) (qs file)))))

(define (filter-data-to-edit obj)
  (alist-delete 'creation-time
                (alist-delete 'modification-time
                              (alist-delete 'id (vault-obj->alist obj)))))

(define (edit-data obj)
  (let ((tmp-file (create-temporary-file)))
    (with-output-to-file tmp-file
      (lambda ()
        (print ";; -*- scheme -*-")
        (pp (filter-data-to-edit obj))))
    (let ((data
           (let loop ()
             (edit-file tmp-file)
             (handle-exceptions exn
               (begin
                 (display "Syntax error.  Edit data again? [y/n] ")
                 (let read-resp ()
                   (let ((resp (read-line)))
                     (cond ((member resp '("y" "Y"))
                            (loop))
                           ((member resp '("n" "N"))
                            (print "Aborting.")
                            (delete-file tmp-file)
                            (exit))
                           (else (read-resp))))))
               (with-input-from-file tmp-file read)))))
      (db-update-object-by-id
       (vault-obj-id obj)
       (make-vault-obj (vault-obj-id obj)
                       (alist-ref 'summary data)
                       (alist-ref 'comment data)
                       (vault-obj-creation-time obj)
                       (current-seconds)
                       (or (alist-ref 'tags data) '())
                       (or (alist-ref 'files data) '())
                       (or (alist-ref 'uris data) '())))
      (delete-file* tmp-file))))

(define-command 'edit "\
edit <id> | -r <regex> [-I] [-e <except regex>]
  Edit objects in the database.  Objects can be selected by id
  or by searching for <regex> in the database.  -I makes search
  case-sensitive.  -e can be provided multiple times and is not affected
  by -I.  Vault uses the editor specified by the VAULT_EDITOR environment
  variable, or the value of the vault-editor configuration parameter,
  in that order."
  (lambda (args)
    (when (null? args)
      (usage 1))
    (let ((case-insensitive? #t)
          (excepts '())
          (regex #f)
          (id #f))
      (cond
       ((null? args)
        (die! "edit: invalid syntax."))
       ((null? (cdr args))
        (let ((maybe-id (string->number (car args))))
          (if maybe-id
              (set! id maybe-id)
              (die! "edit: invalid syntax."))))
       (else
        (let loop ((args args))
          (unless (null? args)
            (let ((arg (car args)))
              (cond ((help-option? arg)
                     (command-usage 'edit 0))
                    ((string=? arg "-I")
                     (set! case-insensitive? #f)
                     (loop (cdr args)))
                    ((string=? arg "-r")
                     (set! regex (parse-option/arg args "-e" regex))
                     (loop (cddr args)))
                    ((string=? arg "-e")
                     (set! excepts (cons (parse-option/arg args "-e") excepts))
                     (loop (cddr args)))
                    (else (die! "edit: invalid syntax."))))))))
      (when (and regex id)
        (die! "<regex> and <id> cannot be used together."))
      (if regex
          (let ((results (db-search regex excepts case-insensitive?)))
            (if (null? results)
                (die! "No match.")
                (let* ((choice (prompt (map (lambda (obj)
                                              (format-vault-obj obj no-id: #t))
                                            results)))
                       (obj (list-ref results choice)))
                  (edit-data obj))))
          (let ((obj (db-get-object-by-id id)))
            (if obj
                (edit-data obj)
                (die! "No such vault object: ~a" id)))))))
