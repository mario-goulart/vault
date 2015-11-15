(define debug-build? (get-environment-variable "VAULT_DEBUG"))

(define make-targets (get-environment-variable "MAKE_TARGETS"))

(define csc-options
  (if debug-build?
      '(-S -O0 -d2)
      '(-O3 -d0)))

(define (mod.so mod)
  (sprintf "~a.so" mod))

(define (mod.scm mod)
  (sprintf "~a.scm" mod))

(define (mod.import.so mod)
  (sprintf "~a.import.so" mod))

(define (mod.import.scm mod)
  (sprintf "~a.import.scm" mod))

(define (mod-rule mod-path mod-deps #!key (non-mod-deps '()))
  (let ((mod.so-deps (cons (mod.scm mod-path)
                           (append (map mod.import.so mod-deps)
                                   non-mod-deps))))
    `((,(mod.so mod-path) ,mod.so-deps
       ,(lambda ()
          (compile -J -s ,@csc-options ,(mod.scm mod-path))))
      (,(mod.import.so mod-path) (,(mod.import.scm mod-path))
       ,(lambda ()
          (compile -s ,@csc-options ,(mod.import.scm mod-path)))))))

(define (command-dep command)
  (make-pathname "commands" (symbol->string command) "scm"))

;; For libraries only.  The command line app deps doesn't belong here.
(define modules/deps
  `((vault-config)
    (vault-mime-types)
    (vault-utils vault-config)
    (vault-db vault-config vault-utils)
    (vault-lib vault-config vault-utils vault-mime-types vault-db
               ,(map command-dep '(del dump list-tags note uri)))
    ))

(define modules (map car modules/deps))

(make-nonfile-targets '("clean" "build" "install" "all"))

(define (cleanup!)
  (for-each delete-file*
            (append
             (list "vault")
             (find-files "." test: (lambda (f)
                                     (string-suffix? "~" f)))
             (find-files "." test: (lambda (f)
                                     (or (equal? (pathname-extension f) "so")
                                         (string-suffix? ".import.scm" f)))))))


(define *rules* '())

(define (add-rule! target deps #!optional proc)
  (set! *rules*
        (cons (if proc
                  (list target deps proc)
                  (list target deps))
              *rules*)))

;; Module rules
(set! *rules*
      (append
       (apply append
              (map (lambda (mod/deps)
                     (mod-rule (car mod/deps)
                               (remove pair? (cdr mod/deps))
                               non-mod-deps: (let ((non-mod
                                                    (filter pair?
                                                            (cdr mod/deps))))
                                               (if (null? non-mod)
                                                   '()
                                                   (car non-mod)))))
                   modules/deps))
       *rules*))

;; Rule for the command line application
(add-rule! "vault" (cons "vault.scm"
                           (map mod.so modules))
           (lambda ()
             (compile ,@csc-options vault.scm)))

;; clean
(add-rule! "clean" '() cleanup!)

;; build
(add-rule! "build" (append (map mod.so modules)
                           (map mod.import.so modules)
                           (list "vault")))

;; install
(add-rule! "install" '("build")
           (lambda ()
             ;; Command line app
             (install-program 'vault
                              '("vault")
                              `((version ,vault-version)))
             ;; Modules
             (for-each (lambda (mod)
                         (install-extension
                          mod
                          (list (mod.import.so mod) (mod.so mod))
                          `((version ,vault-version))))
                       modules)))


;; all
(add-rule! "all" '("install"))

(make/proc *rules* (or make-targets "all"))
