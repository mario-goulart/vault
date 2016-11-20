(module vault-db

(initialize-database
 maybe-migrate-db!
 db-insert-object
 db-dump-objects
 db-list-tags
 db-get-vault-objects
 db-get-vault-object-by-id
 db-delete-object-by-id
 db-search
 db-update-vault-obj)

(import chicken scheme)
(use data-structures extras files irregex posix srfi-1 srfi-13)
(use matchable sql-de-lite ssql)
(use vault-config vault-utils)

(define (db-query/str db-conn q #!key (values '()))
  (apply query (append (list (map-rows (lambda (data) data))
                             (sql db-conn q))
                       values)))

(define (db-query db-conn q)
  (apply query (list (map-rows (lambda (data) data))
                     (sql db-conn (ssql->sql #f q)))))

(define (with-db-transaction proc)
  (call-with-database (db-file)
    (lambda (db)
      (with-transaction db
        (lambda ()
          (proc db))))))

(define (initialize-database db-file)
  (info "Initializing database ~a" db-file)
  (with-output-to-file db-file (cut display ""))
  (let ((db (open-database db-file)))

    ;; Enable foreign keys
    (exec (sql db "pragma foreign_keys = ON"))

    ;; Vault table
    (exec (sql db "
create table vault (
  obj_id integer primary key autoincrement,
  summary text,
  comment text,
  filename text,
  creation_time text,
  modification_time text
)"))

    ;; Tags table (DEPRECATED)
    (exec (sql db "
create table tags (
    tag_id integer primary key autoincrement,
    label text
)"))

    ;; Objs & tags (DEPRECATED)
    (exec (sql db "
create table objs_tags (
    obj_id integer,
    tag_id integer,
    foreign key(obj_id) references vault(obj_id),
    foreign key(tag_id) references tags(tag_id)
)"))

    ;; Version
    (exec (sql db "create table version (version integer)"))
    (exec (sql db "insert into version (version) values (1)"))
    (close-database db)))


(define (db-insert-obj-attr! db obj-id attr-type vals)
  (for-each (lambda (val)
              (db-query db `(insert (into objs_attrs)
                                    (columns obj_id type value)
                                    (values #(,obj-id ,attr-type ,val)))))
            vals))

(define (db-remove-obj-attr! db obj-id attr-type vals)
  (db-query db `(delete (from objs_attrs)
                        (where (and (= obj_id ,obj-id)
                                    (= type ,attr-type)
                                    (in value ,(list->vector vals)))))))

(define (db-get-obj-attr db obj-id attr-type)
  (filter-map
   (lambda (item)
     (if (null? item)
         #f
         (car item)))
   (db-query db `(select (columns value)
                         (from objs_attrs)
                         (where (and (= obj_id ,obj-id)
                                     (= type ,attr-type)))
                         (order (asc value))))))

(define (db-insert-object summary comment tags files uris)
  ;; Insert an object into the database if none with the given summary
  ;; exists; or does nothing if it already exists.  If the object
  ;; already e
  (with-db-transaction
   (lambda (db)
     (db-query db
               `(insert
                 (into vault)
                 (columns summary
                          comment
                          creation_time
                          modification_time)
                 (values #(,summary
                           ,comment
                           |datetime(CURRENT_TIMESTAMP, 'localtime')|
                           |datetime(CURRENT_TIMESTAMP, 'localtime')|))))
     (let ((obj-id (last-insert-rowid db)))
       (db-insert-obj-attr! db obj-id "tag" tags)
       (db-insert-obj-attr! db obj-id "file" files)
       (db-insert-obj-attr! db obj-id "uri" uris)
       obj-id))))

(define (db-get-vault-objects db #!key where)
  (let ((objs (db-query db
                `(select (columns obj_id
                                  summary
                                  comment
                                  creation_time
                                  modification_time)
                         (from vault)
                         ,(if where
                              `(where ,where)
                              '())))))
    (map (lambda (obj)
           (match-let (((obj-id
                         summary
                         comment
                         creation_time
                         modification_time)
                        obj))
             (let ((tags (db-get-obj-attr db obj-id "tag"))
                   (files (db-get-obj-attr db obj-id "file"))
                   (uris (db-get-obj-attr db obj-id "uri")))
               (apply make-vault-obj (list obj-id
                                           summary
                                           comment
                                           creation_time
                                           modification_time
                                           tags
                                           files
                                           uris)))))
         objs)))

(define (db-get-vault-object-by-id id)
  (call-with-database (db-file)
    (lambda (db)
      (let ((obj (db-get-vault-objects db where: `(= obj_id ,id))))
        (if (null? obj)
            #f
            (car obj))))))

(define (db-dump-objects)
  ;; Return a list of vault objects
  (call-with-database (db-file) db-get-vault-objects))

(define (db-list-tags)
  ;; FIXME: todo list count of objects that contain each tag
  (call-with-database (db-file)
    (lambda (db)
      (db-query db
        `(select (distinct (columns value))
                 (from objs_attrs)
                 (where (= type "tag"))
                 (order (asc value)))))))

(define (db-delete-object-by-id . ids)
  (with-db-transaction
   (lambda (db)
     (db-query db
       `(delete (from objs_attrs)
                (where (in obj_id ,(list->vector ids)))))
     (db-query db
       `(delete (from vault)
                (where (in obj_id ,(list->vector ids))))))))

(define (db-search regex excepts case-insensitive?)
  (call-with-database (db-file)
    (lambda (db)
      ;; FIXME: implement without slurping everything into memory
      (let ((objs (db-get-vault-objects db))
            (re (irregex (if case-insensitive?
                             `(w/nocase ,(string->sre regex))
                             regex)))
            (excs (map irregex excepts)))
        (filter-map
         (lambda (obj)
           (let* ((summary% (vault-obj-summary obj))
                  (summary (if (null? summary%) #f summary%))
                  (comment% (vault-obj-comment obj))
                  (comment (if (null? comment%) #f comment%)))
             (and (or (and summary (irregex-search re summary))
                      (and comment (irregex-search re comment)))
                  (not (any (lambda (ex)
                              (or (and summary (irregex-search ex summary))
                                  (and comment (irregex-search ex comment))))
                            excs))
                  obj)))
           objs)))))

(define (db-update-vault-obj obj-id summary comment
                             old-tags new-tags
                             old-files new-files
                             old-uris new-uris)
  (define (val->string val)
    (if (and val (not (null? val)))
        (->string val)
        'null))

  (define (get-removed/added old new)
    (let ((old (map val->string old))
          (new (map val->string new)))
      (list (lset-difference equal? old new)
            (lset-difference equal? new old))))

  (match-let (((removed-tags added-tags) (get-removed/added old-tags new-tags))
              ((removed-files added-files) (get-removed/added old-files new-files))
              ((removed-uris added-uris) (get-removed/added old-uris new-uris)))
    (with-db-transaction
     (lambda (db)
       (db-query db
                 `(update (table vault)
                          (set (summary ,(val->string summary))
                               (comment ,(val->string comment))
                               (modification_time
                                |datetime(CURRENT_TIMESTAMP, 'localtime')|))
                          (where (= obj_id ,obj-id))))
       (db-remove-obj-attr! db obj-id "tag" removed-tags)
       (db-insert-obj-attr! db obj-id "tag" added-tags)
       (db-remove-obj-attr! db obj-id "file" removed-files)
       (db-insert-obj-attr! db obj-id "file" added-files)
       (db-remove-obj-attr! db obj-id "uri" removed-uris)
       (db-insert-obj-attr! db obj-id "uri" added-uris)
       #t))))


;;;
;;; Migration support
;;;

(define *db-migrations* '())

(define (add-migration! next-version migration-proc)
  (let ((migration
         (lambda ()
           (call-with-database (db-file)
             (lambda (db)
               (with-transaction db
                 (lambda ()
                   (migration-proc db)
                   (db-query db '(delete (from version)))
                   (db-query db `(insert (into version)
                                         (columns version)
                                         (values #(,next-version))))
                   #t)))))))
    (set! *db-migrations* (cons migration *db-migrations*))))

(define (current-db-version)
  (call-with-database (db-file)
    (lambda (db)
      (caar (db-query db '(select (columns version) (from version)))))))

(define (maybe-migrate-db!)
  (let ((version (current-db-version))
        (total-migrations (length *db-migrations*)))
    (debug 1 "Current DB schema version is ~S" version)
    (assert (<= version total-migrations))
    (let ((migrations (drop (reverse *db-migrations*) version)))
      (for-each
       (lambda (migration version)
         (let ((next-version (+ version 1)))
           (debug 1 "Backing up the database before running the migration")
           (let* ((db-dir (pathname-directory (db-file)))
                  (db-filename (pathname-strip-directory (db-file)))
                  (old-dbs-dir (make-pathname db-dir "old-dbs")))
             (create-directory old-dbs-dir)
             (file-copy (db-file)
                        (make-pathname old-dbs-dir
                                       (sprintf "~a.~a-~a"
                                                db-filename
                                                (- next-version 1)
                                                next-version))
                        'clobber))
           (info "Running migration from version ~a to ~a..." version next-version)
           (handle-exceptions exn
             (begin
               (print-call-chain (current-error-port))
               (print-error-message exn (current-error-port))
               (problem "Error running migration from version ~a to ~a" version next-version)
               (exit 1))
             (migration))
           (info "Successfully migrated from version ~a to ~a" version next-version)))
       migrations
       (iota (- total-migrations version) version)))))

;;;
;;; Migrations
;;;

;;; IMPORTANT: migrations must be added in the order they are to be
;;; applied!

;; version 0 -> 1
(add-migration! 1 void) ;; The initial version was 1

;; version 1 -> 2
(add-migration!
 2
 (lambda (db)
   ;; type can be: 'uri', 'file' or 'tag'
   (db-query/str db "
create table objs_attrs (
    obj_id integer,
    type text,
    value text,
    foreign key(obj_id) references vault(obj_id)
)")
   (for-each
    (lambda (obj-id)
      (let* ((tags
              (map car
                   (db-query db
                    `(select (columns tags.label)
                             (from tags objs_tags)
                             (where (and (= objs_tags.obj_id ,obj-id)
                                         (= tags.tag_id objs_tags.tag_id)))))))
             (summary/comment/file
              (car (db-query db
                             `(select (columns summary comment filename)
                                      (from vault)
                                      (where (= obj_id ,obj-id))))))
             (summary (car summary/comment/file))
             (comment (cadr summary/comment/file))
             (file (caddr summary/comment/file)))
        ;; Insert file
        (when (and file (not (null? file)))
          (db-query db `(insert (into objs_attrs)
                                (columns obj_id type value)
                                (values #(,obj-id "file" ,file)))))
        ;; Check if summary is an URI.  If it is, insert it as an URI.
        (when (or (string-prefix-ci? "http://" summary)
                  (string-prefix-ci? "https://" summary)
                  (string-prefix-ci? "ftp://" summary)
                  (string-prefix-ci? "gopher://" summary))
          (db-query db `(insert (into objs_attrs)
                                (columns obj_id type value)
                                (values #(,obj-id "uri" ,summary))))
          (let ((comment-is-summary?
                 (not (string-contains comment "\n"))))
            ;(debug 1 "comment-is-summary? ~S, comment: ~S" comment-is-summary? comment)
            (db-query db `(update (table vault)
                                  (set (summary ,(if comment-is-summary?
                                                     comment
                                                     'null))
                                       (comment ,(if comment-is-summary?
                                                     'null
                                                     comment)))
                                  (where (= obj_id ,obj-id))))))
        ;; Insert tags
        (for-each (lambda (tag)
                    (db-query db `(insert (into objs_attrs)
                                          (columns obj_id type value)
                                          (values #(,obj-id "tag" ,tag)))))
                  tags)))
    (map car (db-query db `(select (columns obj_id) (from vault)))))

   (db-query/str db "drop table objs_tags")
   (db-query/str db "drop table tags")))

) ;; end module
