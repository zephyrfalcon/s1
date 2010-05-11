;; s1.scm

(use gauche.parseopt)

(define (abspath path)
  (sys-normalize-pathname path :absolute #t :expand #t :canonicalize #t))

;; load a file relative to the main program.
(define (load-relative name)
  (load name :paths
        (list (sys-dirname (abspath *program-name*)))))

(load-relative "tools")
(load-relative "core")

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
