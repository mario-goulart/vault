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
 help-edit cmd-edit
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
(include "commands/edit.scm")

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

#help-edit

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

(define (format-vault-obj obj #!key no-id)
  (let ((tags (vault-obj-tags obj))
        (comment (vault-obj-comment obj))
        (filename (vault-obj-filename obj))
        (creation-time (vault-obj-creation-time obj))
        (last-modified (vault-obj-modification-time obj)))
    (with-output-to-string
      (lambda ()
        (if no-id
            (print (vault-obj-summary obj))
            (printf "[~a] ~a\n"
                    (vault-obj-id obj)
                    (vault-obj-summary obj)))
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
        (newline)))))

(define (print-vault-obj obj)
  (print (format-vault-obj obj)))

(define (vault-obj->alist obj)
  `((summary . ,(vault-obj-summary obj))
    (comment . ,(vault-obj-comment obj))
    (filename . ,(vault-obj-filename obj))
    (tags . ,(vault-obj-tags obj))))

;; Adapted from chicken-doc (thanks zb)
(define (with-output-to-pager thunk)
  (cond ((get-environment-variable "EMACS")
         (thunk))  ; Don't page in emacs subprocess.
        ((not (terminal-port? (current-output-port)))
         (thunk))  ; Don't page if stdout is not a TTY.
        (else
         (unless (get-environment-variable "LESS")
           (setenv "LESS" "FRXis"))  ; Default 'less' options
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

) ;; end module
