(module vault-lib

(initialize-home
 load-config

 usage
 help-note cmd-note
 help-dump cmd-dump
 help-list-tags cmd-list-tags
 help-uri cmd-uri
 help-del cmd-del
 help-search cmd-search
 )

(import chicken scheme)
(use data-structures extras irregex files posix ports utils)
(use html-parser http-client intarweb simple-sha1 uri-common)
(use vault-config vault-utils vault-mime-types vault-db)

(include "commands/note.scm")
(include "commands/uri.scm")
(include "commands/dump.scm")
(include "commands/list-tags.scm")
(include "commands/del.scm")
(include "commands/search.scm")

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

#help-del

#help-search

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

(define (print-vault-obj obj)
  (let ((tags (vault-obj-tags obj))
        (comment (vault-obj-comment obj))
        (filename (vault-obj-filename obj))
        (creation-time (vault-obj-creation-time obj))
        (last-modified (vault-obj-modification-time obj)))
    (printf "[~a] ~a\n"
            (vault-obj-id obj)
            (vault-obj-summary obj))
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


) ;; end module
