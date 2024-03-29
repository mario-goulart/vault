(module vault-lib

(initialize-home
 load-config
 parse-option/arg
 pathname-strip-download-dir
 print-vault-obj
 save-file
 delete-object-by-id

 ;; for tests
 today-dir
 )

(import scheme)
(cond-expand
  (chicken-5
   (import (chicken base)
           (chicken condition)
           (chicken file)
           (chicken fixnum)
           (chicken format)
           (chicken io)
           (chicken irregex)
           (chicken pathname)
           (chicken platform)
           (chicken port)
           (chicken pretty-print)
           (chicken process)
           (chicken process-context)
           (chicken string)
           (chicken time)
           (chicken time posix))
   (import commands html-parser http-client intarweb simple-sha1 srfi-1 srfi-13 uri-common)
   (import vault-config vault-utils vault-mime-types vault-db))
  (else
   (error "Unsupported CHICKEN version.")))

(include "commands/attach.scm")
(include "commands/note.scm")
(include "commands/uri.scm")
(include "commands/list.scm")
(include "commands/list-tags.scm")
(include "commands/del.scm")
(include "commands/search.scm")
(include "commands/edit.scm")

;;; Initialization
(define (initialize-home)
  (unless (file-exists? (db-file))
    (create-directory (vault-home) 'recursively)
    (initialize-database (db-file)))
  (create-directory (download-dir) 'recursively))

(define (load-config config-file)
  (cond ((and (file-exists? config-file)
              (file-readable? config-file))
         (debug 2 "Loading user configuration file ~a" config-file)
         (load config-file))
        (else
         (debug 2 "User config file doesn't exist or is not readable. Skipping."))))

;;; utils
;;; Command line parsing
(define (parse-option/arg args option #!optional check-multiple)
  (when check-multiple
    (die! "~a cannot be provided multiple times." option))
  (if (null? (cdr args))
      (die! "~a requires an argument." option)
      (cadr args)))

(define (ensure-integer n)
  (and n (inexact->exact n)))

(define (format-vault-obj obj #!key no-id)
  (let ((tags (vault-obj-tags obj))
        (files (vault-obj-files obj))
        (uris (vault-obj-uris obj))
        (summary (vault-obj-summary obj))
        (comment (vault-obj-comment obj))
        (creation-time (ensure-integer (vault-obj-creation-time obj)))
        (last-modified (ensure-integer (vault-obj-modification-time obj))))
    (with-output-to-string
      (lambda ()
        (if no-id
            (when (and summary (not (null? summary)))
              (print summary))
            (printf "[~a] ~a\n" (vault-obj-id obj) (or summary "")))
        (unless (null? tags)
          (printf "  tags: ~S\n" tags))
        (unless (null? uris)
          (print "  URIs:")
          (for-each (lambda (uri)
                      (print "    * " uri))
                    uris))
        (unless (null? files)
          (print "  files:")
          (for-each (lambda (file)
                      (print "    * " (make-pathname (download-dir) file)))
                    files))
        (when (and comment (not (null? comment)))
          (printf "  comment: ~a\n" comment))
        (printf "  creation time: ~a" (seconds->string creation-time))
        (when (and last-modified
                   (not (equal? creation-time last-modified)))
          (newline)
          (printf "  last modified: ~a" (seconds->string last-modified)))))))

(define (print-vault-obj obj)
  (print (format-vault-obj obj)))

;; Adapted from chicken-doc (thanks zb)
(define (with-output-to-pager thunk)
  (cond ((get-environment-variable "EMACS")
         (thunk))  ; Don't page in emacs subprocess.
        ((not (terminal-port? (current-output-port)))
         (thunk))  ; Don't page if stdout is not a TTY.
        (else
         (unless (get-environment-variable "LESS")
           (set-environment-variable! "LESS" "FRXis"))  ; Default 'less' options
         (let ((pager (or (get-environment-variable "VAULT_PAGER")
                          (get-environment-variable "PAGER")
                          (case (software-type)
                            ((windows) "more /s")
                            (else "less")))))
           (if (or (not pager) (string=? pager "cat"))
               (thunk)
               ;; with-output-to-pipe does not close pipe on
               ;; exception, borking tty
               (let ((pipe (open-output-pipe pager))
                     (rv #f))
                 (handle-exceptions exn
                   (begin (close-output-pipe pipe)
                          (signal exn))
                   ;; Can't reliably detect if pipe open fails.
                   (set! rv (with-output-to-port pipe thunk)))
                 (close-output-pipe pipe)
                 rv))))))

(define (prompt options)

  (define (inner-prompt)
    (with-output-to-pager
     (lambda ()
       (let loop ((i 0)
                  (options options))
         (unless (null? options)
           (printf "[~a] ~a\n" i (car options))
           (loop (fx+ i 1) (cdr options))))
       (flush-output)))
    (display "Option (ENTER to abort): ")
    (read-line))

  (let loop ()
    (let ((choice (inner-prompt)))
      (when (equal? choice "")
        (exit))
      (or (string->number choice)
          (begin
            (printf "~a: invalid option.\n" choice)
            (loop))))))

(define (pad-number n zeroes)
  (define (pad num len)
    (let ((str (if (string? num) num (number->string num))))
      (if (string-null? str)
          ""
          (if (>= (string-length str) len)
              str
              (string-pad str len #\0)))))
  (let ((len (string-length (->string n))))
    (if (= len zeroes)
        (number->string n)
        (pad n zeroes))))

(define (today-dir)
  (let* ((now (seconds->local-time))
         (day (pad-number (vector-ref now 3) 2))
         (month (pad-number (add1 (vector-ref now 4)) 2))
         (year (number->string (+ 1900 (vector-ref now 5)))))
    (make-pathname (list year month) day)))


(define (path-separator? char)
  (or (char=? char #\/)
      (and (eqv? (software-type) 'windows)
           (char=? char #\\))))

(define (pathname-strip-download-dir path)
  (let ((path (normalize-pathname path))
        (ddir (normalize-pathname (download-dir))))
    (if (string-prefix? ddir path)
        (let ((relpath (substring path (string-length ddir))))
          (let loop ((relpath relpath))
            (if (string-null? relpath)
                ""
                (if (path-separator? (string-ref relpath 0))
                    (loop (substring relpath 1))
                    relpath))))
        path)))

(define (save-file content content-is-path? #!optional content-type uri)
  ;; Save `content' into the download directory, under a directory
  ;; named after today's date and using the SHA1 sum of `content'.  If
  ;; `content' is a path to a local file, just copy the file,
  ;; otherwise dump it to the output file.  If `content' is a file,
  ;; `content-type' and `uri' are not used.
  ;; Return the path to the file in the download directory.
  (let* ((out-dir (make-pathname (download-dir) (today-dir)))
         (out-file
          (make-pathname out-dir
                         (if content-is-path?
                             (sha1sum content)
                             (string->sha1sum content))
                         (if content-is-path?
                             (pathname-extension content)
                             (mime-type->extension content-type)))))
    (create-directory out-dir 'recursively)
    (debug 1 "Writing ~a to ~a" (if content-is-path? content uri) out-file)
    (if content-is-path?
        (copy-file content out-file 'clobber)
        (with-output-to-file out-file
          (cut display content)))
    out-file))

(define (delete-object-by-id id)
  (let ((obj (db-get-object-by-id id)))
    (when obj
      (let ((files (vault-obj-files obj)))
        (for-each
         (lambda (file)
           (let ((obj-ids-owning-file (db-get-object-ids-linked-to-file file)))
             ;; obj-ids-owning-file contains at least `id'
             (if (null? (cdr obj-ids-owning-file))
                 (begin
                   (info "Deleting file ~a linked to object ~a" file id)
                   (delete-file* (make-pathname (download-dir) file)))
                 (info "Keeping file ~a as it is linked to the following objects: ~a"
                       file
                       (string-intersperse
                        (map number->string (delete id obj-ids-owning-file))
                        ", ")))))
         files)
        (db-delete-object-by-id id)))))

) ;; end module
