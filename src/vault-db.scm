(module vault-db

(initialize-database
 db-insert-object
 db-dump-objects
 db-list-tags
 db-get-vault-objects
 db-delete-object-by-id
 db-search)

(import chicken scheme)
(use irregex srfi-1 srfi-13)
(use matchable sql-de-lite ssql)
(use vault-config vault-utils)

;; (define (db-query db-conn q #!key (values '()))
;;   (apply query (append (list (map-rows (lambda (data) data))
;;                              (sql db-conn q))
;;                        values)))

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

    ;; Tags table
    (exec (sql db "
create table tags (
    tag_id integer primary key autoincrement,
    label text
)"))

    ;; Objs & tags
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

(define (db-tag-exists? db label)
  (not (null? (db-query db
                `(select (columns tag_id)
                         (from tags)
                         (where (= label ,label)))))))

(define (db-insert-tag db label)
  (unless (db-tag-exists? db label)
    (db-query db
      `(insert (into tags) (columns label) (values #(,label))))))

(define (db-object-linked-to-tag? db obj-id tag)
  (not (null?
        (db-query db
          `(select (columns objs_tags.obj_id objs_tags.tag_id)
                   (from objs_tags tags)
                   (where (and (= objs_tags.obj_id ,obj-id)
                               (= tags.label ,tag)
                               (= objs_tags.tag_id tags.tag_id))))))))

(define (db-link-object-tags db obj-id tags)
  (for-each
   (lambda (tag)
     (db-insert-tag db tag)
     (unless (db-object-linked-to-tag? db obj-id tag)
       (db-query db
         `(insert (into objs_tags)
                  (columns obj_id tag_id)
                  (values #(,obj-id
                            (select (columns tag_id)
                                    (from tags)
                                    (where (= label ,tag)))))))))
   (map string-trim-both tags)))

(define (db-object-exists? db summary)
  (not (null? (db-query db
                `(select (columns summary)
                         (from vault)
                         (where (= summary ,summary)))))))

(define (db-insert-object summary comment filename tags)
  (let* ((exists? #f)
         (result
          (with-db-transaction
           (lambda (db)
             (if (db-object-exists? db summary)
                 (set! exists? #t)
                 (begin
                   (db-query db
                     `(insert
                       (into vault)
                       (columns summary
                                comment
                                filename
                                creation_time
                                modification_time)
                       (values #(,summary
                                 ,comment
                                 ,filename
                                 |datetime(CURRENT_TIMESTAMP, 'localtime')|
                                 |datetime(CURRENT_TIMESTAMP, 'localtime')|))))
                   (let ((obj-id (last-insert-rowid db)))
                     (db-link-object-tags db obj-id tags))))
             #t))))
    (if exists?
        -1
        result)))

(define (db-object-tags db obj-id)
  (map car
       (db-query db
         `(select (columns tags.label)
                  (from tags objs_tags)
                  (where (and (= objs_tags.obj_id ,obj-id)
                              (= tags.tag_id objs_tags.tag_id)))
                  (order (asc label))))))

(define (db-get-vault-objects db #!key where)
  (let ((objs (db-query db
                `(select (columns obj_id
                                  summary
                                  comment
                                  filename
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
                         filename
                         creation_time
                         modification_time)
                        obj))
             (let ((tags (db-object-tags db obj-id)))
               (apply make-vault-obj (list obj-id
                                           summary
                                           comment
                                           filename
                                           creation_time
                                           modification_time
                                           tags)))))
         objs)))

(define (db-dump-objects)
  ;; Return a list of vault objects
  (call-with-database (db-file) db-get-vault-objects))

(define (db-list-tags)
  ;; FIXME: todo list count of objects that contain each tag
  (call-with-database (db-file)
    (lambda (db)
      (db-query db
        `(select (distinct (columns label))
                 (from tags)
                 (order (asc label)))))))

(define (db-delete-object-by-id . ids)
  (call-with-database (db-file)
    (lambda (db)
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
           (and (or (irregex-search re (vault-obj-summary obj))
                    (irregex-search re (vault-obj-comment obj)))
                (not (any (lambda (ex)
                            (or (irregex-search ex (vault-obj-summary obj))
                                (irregex-search ex (vault-obj-comment obj))))
                          excs))
                obj))
         objs)))))

) ;; end module
