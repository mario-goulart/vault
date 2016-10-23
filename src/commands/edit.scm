(define help-edit
  #<#EOF
edit [-I] [-e <except regex>] <regex> | -id <object id>
  Edit objects in the database.  Objects can be selected by id (-id)
  or by searching for <regex> in the database.  -I makes search
  case-sensitive.  -e can be provided multiple times and is not affected
  by -I.  Vault uses the editor specified by the VAULT_EDITOR environment
  variable, or the value of the vault-editor configuration parameter,
  in that order.

EOF
)

(define (edit-file file)
  (handle-exceptions exn
    (begin
      (delete-file file)
      (exit 1))
    (system* (sprintf "~a ~a" (vault-editor) (qs file)))))

(define (edit-data obj)
  (let ((tmp-file (create-temporary-file)))
    (with-output-to-file tmp-file
      (lambda ()
        (print ";; -*- scheme -*-")
        (pp (vault-obj->alist obj))))
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
      (db-update-vault-obj (vault-obj-id obj)
                           (alist-ref 'summary data eq? '())
                           (alist-ref 'comment data eq? '())
                           (alist-ref 'filename data eq? '())
                           (vault-obj-tags obj)
                           (alist-ref 'tags data eq? '()))
      (delete-file* tmp-file))))

(define (cmd-edit args)
  (when (null? args)
    (usage 1))
  (let ((case-insensitive? #t)
        (excepts '())
        (regex '())
        (id '()))
    (let loop ((args args))
      (unless (null? args)
        (let ((arg (car args)))
          (cond ((string=? arg "-I")
                 (set! case-insensitive? #f)
                 (loop (cdr args)))
                ((string=? arg "-e")
                 (when (null? (cdr args))
                   (die! "-e requires an argument."))
                 (set! excepts (cons (cadr args) excepts))
                 (loop (cddr args)))
                ((string=? arg "-id")
                 (when (null? (cdr args))
                   (die! "-id requires an argument."))
                 (set! id (cons (cadr args) id))
                 (loop (cddr args)))
                (else
                 (set! regex (cons arg regex))
                 (loop (cdr args)))))))
    (when (and (not (null? regex))
               (not (null? id)))
      (die! "Search and object id specification (-is) cannot be used toghether."))
    (when (and (null? id)
               (or (null? regex)
                   (not (null? (cdr regex)))))
      (die! "Exactly one regex must be provided."))
    (if (null? id)
        (let ((results (db-search (car regex) excepts case-insensitive?)))
          (if (null? results)
              (die! "No match.")
              (let* ((choice (prompt (map (lambda (obj)
                                            (format-vault-obj obj no-id: #t))
                                          results)))
                     (obj (list-ref results choice)))
                (edit-data obj))))
        (let ((nid (string->number (car id))))
          (if nid
              (let ((obj (db-get-vault-object-by-id nid)))
                (if (null? obj)
                    (die! "No such vault object: ~a" id)
                    (edit-data obj)))
              (die! "Invalid vault object id: ~a" id))))))
