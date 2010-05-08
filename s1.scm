;; s1.scm

(use gauche.parseopt)
(load "./core")

(define (main args)
  (let-args
   (cdr args)
   ((expr-src "e|expr")
    (filename "f|filename")
    . restargs)
   (begin
     (unless (or expr-src filename)
       (set! expr-src (list default-expr)))
     ;; XXX refactor:
     (receive (before-exprs after-exprs exprs)
         (parse-exprs expr-src)
       (process-exprs before-exprs)
       (process-file (current-input-port) exprs) ;; use stdin for now
       (process-exprs after-exprs))
     0)))

#|
proposed command line options:
-e expr     read code from expr
-f file     read code from file
(either -e or -f is required, or it will just pipe stdin to stdout)

"regular" args are assumed to be names of files with data to be read/processed.
|#
