;; s1.scm

(use gauche.parseopt)

;;; --- special variables ---

(define *current-line* #f)

(define *fields* '()) ;; fields in the current line

(define fs #f)        ;; field separator; if undefined, use whitespace
(define ls "\n")      ;; line separator; if undefined, use newline
(define ofs " ")      ;; output file separator
(define ols "\n")     ;; output line separator

(define default-expr
  '(format #t "~a~%" *current-line*))

(define (is-before-expr? expr)
  (and (list? expr)
       (not (null? expr))
       (member (car expr) '(b before B BEFORE))))

(define (is-after-expr? expr)
  (and (list? expr)
       (not (null? expr))
       (member (car expr) '(a after A AFTER))))

(define (parse-exprs sexprs)
  (let loop ((sexprs sexprs) (before-exprs '()) (after-exprs '()) (exprs '()))
    (cond
     ((null? sexprs) ;; done?
      (values (reverse before-exprs)
              (reverse after-exprs)
              (reverse exprs)))

     ((is-before-expr? (car sexprs))
      (loop (cdr sexprs)
            (cons (car sexprs) before-exprs)
            after-exprs
            exprs))

     ((is-after-expr? (car sexprs))
      (loop (cdr sexprs)
            before-exprs
            (cons (car sexprs) after-exprs)
            exprs))

     (else (loop (cdr sexprs)
                 before-exprs after-exprs (cons (car sexprs) exprs))))))

(define (read-data port)
  ;; read all data from the port, and return it as a string.
  (with-input-from-port port
    (lambda () (port->string port))))

(define (split-data data)
  (string-split data ls))

(define (process-exprs exprs)
  ;; evaluate a list of expressions.
  (for-each (cut eval <> #f) exprs))

(define (process-line line exprs)
  (set! *current-line* line)
  ;; also set *fields*, etc
  (process-exprs exprs))

(define (process-file port exprs)
  (let* ((data (read-data port))
         (lines (split-data data)))
    (for-each (cut process-line <> exprs) lines)))

(define (main args)
  (let-args
   (cdr args)
   ((expr-src "e|expr")
    (filename "f|filename")
    . restargs)
   (begin
     (unless (or expr-src filename)
       (set! expr-src (list default-expr)))
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
