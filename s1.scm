;; s1.scm

(use gauche.parseopt)

;; fix *load-path* so we can see files in the same directory :-/
(push! *load-path*
       (sys-dirname (sys-normalize-pathname *program-name* :absolute #t)))

(load "tools")
(load "core")

(define (determine-exprs expr-src filename)
  (cond
   ((string? expr-src)
    (read-exprs-from-string expr-src))
   ((string? filename)
    (read-exprs-from-file filename))
   (else (list default-expr))))

(define (main args)
  (let-args
   (cdr args)
   ((expr-src "e|expr=s")
    (filename "f|filename=s")
    . restargs)
   (begin
     (let ((exec-exprs (determine-exprs expr-src filename)))
       ;; XXX refactor:
       (receive (before-exprs after-exprs exprs)
           (parse-exprs exec-exprs)
         (process-exprs before-exprs)
         (process-file (current-input-port) exprs) ;; use stdin for now
         (process-exprs after-exprs))
       0))))

#|
proposed command line options:
-e expr     read code from expr
-f file     read code from file
(either -e or -f is required, or it will just pipe stdin to stdout)

"regular" args are assumed to be names of files with data to be read/processed.
|#
