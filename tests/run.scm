(use files extras posix setup-api)
(use sql-de-lite test)
(use vault-config vault-utils vault-db vault-lib)

(define vault
  (make-pathname
   (if (get-environment-variable "SALMONELLA_RUNNING")
       #f ;; salmonella adds its REPO_PREFIX/bin to PATH
       (program-path))
   "vault"))

(define (run-vault args)
  (system* (sprintf "~a ~a" vault args)))

(test-begin "vault")

(setenv "VAULT_CONFIG" (make-pathname (current-directory) "vault.conf"))
(load "vault.conf")

(define vault-test-dir (make-pathname (current-directory) "vault-test"))

(when (directory-exists? vault-test-dir)
  (delete-directory vault-test-dir 'recursively))

(define db #f) ;; will be set by the first test that calls run-vault

(on-exit
 (lambda ()
   (close-database db)))

(test "Checking db file"
      (make-pathname vault-test-dir "vault.db")
      (db-file))

(test "Checking download dir"
      (make-pathname vault-test-dir "downloads")
      (download-dir))

(test "simple comment"
      '("a note" "a comment" ("tag1" "tag2"))
      (begin
        (run-vault "note 'a note' -c 'a comment' -t tag1 -t tag2")
        (set! db (open-database (db-file)))
        (let ((objs (db-get-vault-objects db where: `(= obj_id 1))))
          (assert (not (null? objs)))
          (assert (null? (cdr objs)))
          (let ((obj (car objs)))
            (list (vault-obj-summary obj)
                  (vault-obj-comment obj)
                  (vault-obj-tags obj))))))

(test "another simple comment"
      '("another note" "another comment" ("tag1" "tag3"))
      (begin
        (run-vault "note 'another note' -c 'another comment' -t tag1 -t tag3")
        (let ((objs (db-get-vault-objects db where: `(= obj_id 2))))
          (assert (not (null? objs)))
          (assert (null? (cdr objs)))
          (let ((obj (car objs)))
            (list (vault-obj-summary obj)
                  (vault-obj-comment obj)
                  (vault-obj-tags obj))))))

(test-group
 "download uri (html)"
 (test "Checking uri data"
       '("Mario Domenech Goulart" ("mario") ("http://parenteses.org/mario"))
       (begin
         (run-vault "uri http://parenteses.org/mario -t mario")
         (let ((objs (db-get-vault-objects db where: `(= obj_id 3))))
           (assert (not (null? objs)))
           (assert (null? (cdr objs)))
          (let ((obj (car objs)))
            (list (vault-obj-summary obj)
                  (vault-obj-tags obj)
                  (vault-obj-uris obj)))))))

(test-group
 "download uri (png)"
 (test "Checking uri data"
       '(("http://parenteses.org/mario/misc/fail.png") "fail" ("mario"))
       (begin
         (run-vault "uri http://parenteses.org/mario/misc/fail.png -c fail -t mario")
         (let ((objs (db-get-vault-objects db where: `(= obj_id 4))))
           (assert (not (null? objs)))
           (assert (null? (cdr objs)))
          (let ((obj (car objs)))
            (list (vault-obj-uris obj)
                  (vault-obj-comment obj)
                  (vault-obj-tags obj))))))
 (test-assert "Checking the downloaded file (png)"
              (file-exists?
               (make-pathname (list (download-dir) (today-dir))
                              "1f1b025eeec7ccaee81ccef47be092c68bfb2975.png"))))

(test "Checking tags so far"
      '("mario" "tag1" "tag2" "tag3")
      (map car (db-list-tags)))

(delete-directory vault-test-dir 'recursively)

(test-end "vault")
