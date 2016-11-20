(define help-uri
  #<#EOF
uri <URI> [-c <comment>] [-t <tag>] [-u <extra uri>] [-D] [-T] [-s <summary>]
  Add <URI>.  If <URI> is an HTML page and -T or -s are not provided,
  this command will automatically prepend the HTML page title to
  <comment> (-T disables this behavior).  If <URI>'s content type is in
  the `downloadable-mime-types' list (a configurable parameter), this
  command will download it and save it under #(download-dir), unless -D
  is given.  -u and -t may be specified multiple times.
EOF
)

(define (parse-title data)
  (and-let* ((m (irregex-search "<title[^>]*>(.*)</title>"
                                (string-translate* data '(("\n" . "")))))
             ((irregex-match-valid-index? m 1)))
    (let ((title (irregex-match-substring m 1)))
      ;; Handle multiple <title> tags
      (and-let* ((i (substring-index "</title>" title)))
        (set! title (string-take title i)))
      (with-input-from-string
          (string-trim-both title)
        html-strip))))

(define (cmd-uri args)
  (when (null? args)
    (usage 1))
  (let ((uris (list (car args)))
        (tags '())
        (comment #f)
        (page-title #f)
        (download? #t)
        (out-file #f)
        (user-summary #f)
        (use-page-title? #t))
    (let loop ((args (cdr args)))
      (unless (null? args)
        (let ((arg (car args)))
          (cond ((string=? arg "-c")
                 (set! comment (parse-option/arg args "-c" comment))
                 (loop (cddr args)))
                ((string=? arg "-t")
                 (set! tags (cons (parse-option/arg args "-t") tags))
                 (loop (cddr args)))
                ((string=? arg "-u")
                 (set! uris (cons (parse-option/arg args "-u") uris))
                 (loop (cddr args)))
                ((string=? arg "-s")
                 (set! use-page-title? #f)
                 (set! user-summary (parse-option/arg args "-s"))
                 (loop (cddr args)))
                ((string=? arg "-D")
                 (set! download? #f)
                 (loop (cdr args)))
                ((string=? arg "-T")
                 (set! use-page-title? #f)
                 (loop (cdr args)))
                (else (die! "uri: invalid syntax"))))))
    ;; Keep the primary URI as the car
    (set! uris (reverse uris))
    (let ((primary-uri (car uris)))
      (when (or use-page-title? download?)
        (handle-exceptions exn
          (begin
            (warn "Could not download ~a." primary-uri)
            (print-error-message exn))
          (call-with-input-request*
           (make-request uri: (uri-reference primary-uri))
           #f
           (lambda (port response)
             (let* ((headers (response-headers response))
                    (content-type (header-value 'content-type headers 'unknown))
                    (data (cond ((and download?
                                      (memq content-type
                                            (downloadable-mime-types)))
                                 (debug 1 "Downloading ~a..." primary-uri)
                                 (read-string #f port))
                                (use-page-title?
                                 (and (memq content-type (web-page-mime-types))
                                      (begin
                                        (debug 1 "Reading ~a..." primary-uri)
                                        (parse-title
                                         (read-string 10240 port)))))
                                (else #f))))
               (when data
                 (if (and use-page-title?
                          (memq content-type (web-page-mime-types)))
                     (set! page-title data)
                     (begin
                       (set! out-file
                         (make-pathname (download-dir)
                                        (string->sha1sum data)
                                        (mime-type->extension content-type)))
                       (debug 1 "Writing ~a to ~a" primary-uri out-file)
                       (with-output-to-file out-file
                         (cut display data)))))))))))
      (let* ((summary (or user-summary page-title 'null))
             (files (if out-file
                        (list (pathname-strip-directory out-file))
                        '()))
             (obj-id
              (db-insert-object summary (or comment 'null) tags files uris)))
        (print-vault-obj (db-get-vault-object-by-id obj-id)))))
