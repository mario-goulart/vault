(module vault-lib

(initialize-home
 load-config

 usage
 help-note cmd-note
 help-dump cmd-dump
 help-list-tags cmd-list-tags
 help-uri cmd-uri
 )

(import chicken scheme)
(use data-structures extras irregex files posix utils)
(use http-client intarweb simple-sha1 uri-common)
(use vault-config vault-utils vault-mime-types vault-db)


;;; Initialization
(define (initialize-home)
  (unless (file-exists? (db-file))
    (create-directory (vault-home) 'recusively)
    (initialize-database (db-file)))
  (create-directory (download-dir) 'recursively))

(define (load-config config-file)
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

#help-list-tags

#help-uri

EOF
             port)
    (when exit-code
      (exit exit-code))))

;;; utils
(define (parse-comment-option args comment-exists?)
  (when comment-exists?
    (die! "-c cannot be provided multiple times"))
  (if (null? (cdr args))
      (die! "-c requires an argument.")
      (cadr args)))

(define (parse-tag-option args)
  (if (null? (cdr args))
      (die! "-t requires an argument.")
      (cadr args)))

;;; note
(define help-note
  #<#EOF
note <summary> [-c <comment>] [-t <tag>]
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
                 (set! comment (parse-comment-option args comment))
                 (loop (cddr args)))
                ((string=? arg "-t")
                 (set! tags (cons (parse-tag-option args) tags))
                 (loop (cddr args)))
                (else (die! "note: invalid syntax"))))))
    (db-insert-object summary (or comment 'null) 'null tags)))


;;; uri
(define help-uri
  #<#EOF
uri <URI> [-c <comment>] [-t <tag>] [-D] [-T]
  Add <URI>.  If <URI> is an HTML page and -T is not provided, this
  command will automatically prepend the HTML page title to
  <comment> (-T disables this behavior).  If <URI>'s content type is in
  the `downloadable-mime-types' list (a configurable parameter), this
  command will download it and save it under #(download-dir), unless
  -D is given.  -t may be specified multiple times.

EOF
)

(define (parse-title data)
  (and-let* ((m (irregex-search "<title>([^>]+)</title>" data))
             ((irregex-match-valid-index? m 1)))
    (irregex-match-substring m 1)))

(define (cmd-uri args)
  (when (null? args)
    (usage 1))
  (let ((uri (car args))
        (tags '())
        (comment #f)
        (page-title #f)
        (download? #t)
        (out-file #f)
        (use-page-title? #t))
    (let loop ((args (cdr args)))
      (unless (null? args)
        (let ((arg (car args)))
          (cond ((string=? arg "-c")
                 (set! comment (parse-comment-option args comment))
                 (loop (cddr args)))
                ((string=? arg "-t")
                 (set! tags (cons (parse-tag-option args) tags))
                 (loop (cddr args)))
                ((string=? arg "-D")
                 (set! download? #f)
                 (loop (cdr args)))
                ((string=? arg "-T")
                 (set! use-page-title? #f)
                 (loop (cdr args)))
                (else (die! "uri: invalid syntax"))))))
    (when (or use-page-title? download?)
      (call-with-input-request*
       (make-request uri: (uri-reference uri))
       #f
       (lambda (port headers)
         (let* ((content-type (header-value 'content-type headers 'unknown))
                (data (cond ((and download?
                                  (memq content-type
                                        (downloadable-mime-types)))
                             (debug 1 "Downloading ~a..." uri)
                             (read-string #f port))
                            (use-page-title?
                             (and (memq content-type (web-page-mime-types))
                                  (begin
                                    (debug 1 "Reading ~a..." uri)
                                    (parse-title (read-string 2048 port)))))
                            (else #f))))
           (when data
             (if (and use-page-title? (memq content-type (web-page-mime-types)))
                 (set! page-title data)
                 (begin
                   (set! out-file
                         (make-pathname (download-dir)
                                        (string->sha1sum data)
                                        (mime-type->extension content-type)))
                   (debug 1 "Writing ~a to ~a" uri out-file)
                   (with-output-to-file out-file
                     (cut display data)))))
           (db-insert-object uri
                             (cond ((and page-title comment)
                                    ;; FIXME: use markdown
                                    (string-append page-title "\n\n" comment))
                                   (else (or page-title comment "")))
                             (if out-file
                                 (pathname-strip-directory out-file)
                                 'null)
                             tags)))))))


;;; dump
(define help-dump
  #<#EOF
dump
  Dump the database content.
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

;;; list-tags
(define help-list-tags
  #<#EOF
list-tags
  List tags.
EOF
)

(define (cmd-list-tags)
  (for-each (lambda (tag)
              (print (car tag)))
            (db-list-tags)))

) ;; end module
