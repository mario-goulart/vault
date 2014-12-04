(module vault-db

(initialize-database
 db-insert-object)

(import chicken scheme)
(use vault-config vault-utils)
(use sql-de-lite ssql)

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
    tag_id,
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
    (close-database db)))


(define (db-insert-object summary comment filename tags)
  (with-db-transaction
   (lambda (db)
     (db-query db
      `(insert
        (into vault)
        (columns summary comment filename creation_time modification_time)
        (values #(,summary ,comment ,filename CURRENT_TIMESTAMP CURRENT_TIMESTAMP))))
     #t)))

) ;; end module
