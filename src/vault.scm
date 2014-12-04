(module vault-cmd ()

(import chicken scheme)
(use data-structures extras files posix utils)
(use vault-lib vault-db)

(initialize-home)
(load-config)

(db-insert-object "foo" "bar" #f '("tag1" "tag2"))


) ;; end module
