(import scheme)
(cond-expand
  (chicken-5
   (import (chicken file)
           (chicken format)
           (chicken pathname)
           (chicken platform)
           (chicken process)
           (chicken process-context))
   (import vault-config vault-utils vault-db vault-lib)
   (import test)
   (define vault "vault"))
  (else
   (error "Unsupported CHICKEN version.")))

(define (run-vault args)
  (system* (sprintf "~a ~a >/dev/null" vault args)))

(test-begin "vault")

(set-environment-variable! "VAULT_CONFIG"
                           (make-pathname (current-directory) "vault.conf"))
(load "vault.conf")

(define vault-test-dir (make-pathname (current-directory) "vault-test"))

(when (directory-exists? vault-test-dir)
  (delete-directory vault-test-dir 'recursively))

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
        (db (db-read))
        (assert (not (null? (db))))
        (assert (null? (cdr (db))))
        (let ((obj (cdar (db))))
          (list (vault-obj-summary obj)
                (vault-obj-comment obj)
                (vault-obj-tags obj)))))

(test "another simple comment"
      '("another note" "another comment" ("tag1" "tag3"))
      (begin
        (run-vault "note 'another note' -c 'another comment' -t tag1 -t tag3")
        (db (db-read))
        (assert (= 2 (length (db))))
        (let ((obj (cdr (cadr (db)))))
          (list (vault-obj-summary obj)
                (vault-obj-comment obj)
                (vault-obj-tags obj)))))

(delete-directory vault-test-dir 'recursively)

(test-end "vault")
