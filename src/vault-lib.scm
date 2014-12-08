(module vault-lib

(initialize-home
 load-config

 usage
 help-note cmd-note
 help-dump cmd-dump
 )

(import chicken scheme)
(use data-structures extras files posix utils)
(use vault-config vault-utils vault-db)

;;; Constants
(define config-file
  (make-pathname (get-environment-variable "HOME") ".vault.conf"))


;;; Initialization
(define (initialize-home)
  (unless (file-exists? (db-file))
    (create-directory (vault-home) 'recusively)
    (initialize-database (db-file)))
  (create-directory (download-dir) 'recursively))

(define (load-config)
  (cond ((file-read-access? config-file)
         (debug 2 "Loading user configuration file ~a" config-file)
         (load config-file))
        (else
         (debug 2 "User config file doesn't exist or is not readable. Skipping."))))

;;;
;;; Commands
;;;

(define (usage #!optional exit-code)
  (let ((this (pathname-strip-directory (program-name)))
        (port (if (and exit-code (not (zero? exit-code)))
                  (current-error-port)
                  (current-output-port))))
    (display #<#EOF
Usage: #this <command> [<options>]

<commands>:

#help-note

#help-dump

EOF
             port)
    (when exit-code
      (exit exit-code))))

;;; note
(define help-note
  #<#EOF
note <summary> -c <comment> -t <tag>
  Add note with <summary> as summary. -c can be used to specify a
  comment and -t can be used to specify tags (may be used multiple times).
EOF
)

(define (cmd-note args)
  (when (null? args)
    (usage 1))
  (let ((summary (car args))
        (tags '())
        (comment #f))
    (let loop ((args (cdr args)))
      (unless (null? args)
        (let ((arg (car args)))
          (cond ((string=? arg "-c")
                 (when comment
                   (die! "-c cannot be provided multiple times"))
                 (if (null? (cdr args))
                     (die! "-c requires an argument.")
                     (begin
                       (set! comment (cadr args))
                       (loop (cddr args)))))
                ((string=? arg "-t")
                 (if (null? (cdr args))
                     (die! "-t requires an argument.")
                     (begin
                       (set! tags (cons (cadr args) tags))
                       (loop (cddr args)))))
                (else (die! "note: invalid syntax"))))))
    (db-insert-object summary (or comment 'null) 'null tags)))


;;; dump
(define help-dump
  #<#EOF
dump
  Dump the database content
EOF
)

(define (cmd-dump)
  (for-each
   (lambda (obj)
     (let ((tags (vault-obj-tags obj))
           (comment (vault-obj-comment obj))
           (filename (vault-obj-filename obj))
           (creation-time (vault-obj-creation-time obj))
           (last-modified (vault-obj-modification-time obj)))
       (printf "* ~a\n" (vault-obj-summary obj))
       (unless (null? tags)
         (printf "  tags: ~S\n" tags))
       (unless (null? comment)
         (printf "  comment: ~a\n" comment))
       (unless (null? filename)
         (printf "  filename: ~a\n"
                 (make-pathname (download-dir) filename)))
       (printf "  creation time: ~a\n" creation-time)
       (unless (equal? creation-time last-modified)
         (printf "  last modified: ~a\n" last-modified))
       (newline)))
   (db-dump-objects)))

) ;; end module
