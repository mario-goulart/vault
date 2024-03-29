(module vault-db

(initialize-database
 db
 db-read
 db-write!
 db-insert-object
 db-list-tags
 db-search
 db-get-object-ids-linked-to-file
 db-get-object-by-id
 db-update-object-by-id
 db-delete-object-by-id)

(import scheme)
(cond-expand
  (chicken-5
   (import (chicken base)
           (chicken file)
           (chicken io)
           (chicken irregex)
           (chicken pathname)
           (chicken pretty-print)
           (chicken process-context)
           (chicken sort)
           (chicken time))
   (import srfi-1 srfi-13)
   (import vault-config vault-utils))
  (else
   (error "Unsupported CHICKEN version.")))

(define db (make-parameter #f))

(define (db-read)
  (let ((objs (with-input-from-file (db-file) read-list)))
    (map cons
         (map (lambda (obj) (alist-ref 'id obj)) objs)
         (map alist->vault-obj objs))))

(define (initialize-database db-file)
  (info "Initializing database ~a" db-file)
  (with-output-to-file db-file (cut print "")))

(define (db-get-next-id)
  (let loop ((objs (db))
             (last-id 0))
    (if (null? objs)
        (add1 last-id)
        (let* ((id (caar objs)))
          (loop (cdr objs)
                (if (> id last-id)
                    id
                    last-id))))))

(define (db-insert-object summary comment tags files uris)
  (let* ((new-id (db-get-next-id))
         (new-item `((id . ,new-id)
                     (summary . ,summary)
                     (comment . ,comment)
                     (creation-time . ,(current-seconds))
                     (tags . ,(sort tags string<))
                     (files . ,files)
                     (uris . ,uris))))
    (db (if (null? db)
            new-item
            (append (db)
                    (list (cons new-id (alist->vault-obj new-item))))))
    new-id))

(define (safe-update-file! file thunk)
  ;; Warning: this might leak temporary files
  (let ((old-tmpdir (get-environment-variable "TMPDIR")))
    (set-environment-variable! "TMPDIR" (pathname-directory file))
    (let ((tmp-file (create-temporary-file "vault.tmp")))
      (with-output-to-file tmp-file thunk)
      (rename-file tmp-file file 'clobber))
    (if old-tmpdir
        (set-environment-variable! "TMPDIR" old-tmpdir)
        (unset-environment-variable! "TMPDIR"))))

(define (db-write!)
  (safe-update-file!
   (db-file)
   (lambda ()
     (for-each
      (lambda (obj)
        (pp (vault-obj->alist obj)))
      (map cdr (db)))))

  (when (tags-cache-file)
    (safe-update-file!
     (tags-cache-file)
     (lambda ()
       (for-each print (db-list-tags))))))

(define (db-get-object-by-id id)
  (let loop ((db (db)))
    (if (null? db)
        #f
        (let ((id/obj (car db)))
          (if (= (car id/obj) id)
              (cdr id/obj)
              (loop (cdr db)))))))

(define (db-update-object-by-id id new-obj)
  (db
   (let loop ((db (db)))
     (if (null? db)
         '()
         (let ((id/obj (car db)))
           (cons
            (if (= (car id/obj) id)
                (cons id new-obj)
                id/obj)
            (loop (cdr db))))))))

(define (db-search regex excepts case-insensitive?)
  (let ((objs (map cdr (db)))
        (re (irregex (if case-insensitive?
                         `(w/nocase ,(string->sre regex))
                         regex)))
        (excs (map irregex excepts)))
    (filter-map
     (lambda (obj)
       (let* ((summary% (vault-obj-summary obj))
              (summary (if (or (not summary%) (null? summary%))
                           #f
                           summary%))
              (comment% (vault-obj-comment obj))
              (comment (if (or (not comment%) (null? comment%))
                           #f
                           comment%)))
         (and (or (and summary (irregex-search re summary))
                  (and comment (irregex-search re comment)))
              (not (any (lambda (ex)
                          (or (and summary (irregex-search ex summary))
                              (and comment (irregex-search ex comment))))
                        excs))
              obj)))
     objs)))

(define (db-list-tags)
  (sort
   (let loop ((db (map cdr (db)))
              (tags '()))
     (if (null? db)
         tags
         (let ((obj (car db)))
           (loop (cdr db)
                 (lset-union equal? tags (vault-obj-tags obj))))))
   string<))

(define (db-get-object-ids-linked-to-file file)
  (let loop ((db (map cdr (db)))
             (ids '()))
    (if (null? db)
        ids
        (let ((obj (car db)))
          (loop (cdr db)
                (if (member file (vault-obj-files obj))
                    (cons (vault-obj-id obj) ids)
                    ids))))))

(define (db-delete-object-by-id id)
  (db (remove
       (lambda (id/obj)
         (= (car id/obj) id))
       (db))))

) ;; end module
