;; core.scm

(load "./tools")

;;; --- special variables ---

(define *current-line* #f)

(define *fields* '()) ;; fields in the current line

(define fs #/[ \n\t\r]+/) ;; field separator; if undefined, use whitespace

(define ls "\n")      ;; line separator; if undefined, use newline
(define ofs " ")      ;; output file separator
(define ols "\n")     ;; output line separator

;;; --- handling Scheme expressions ---

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

(define (normalize-before/after-expr expr)
  ;; replace (BEFORE ...) with (begin ...), etc.
  (cons 'begin (cdr expr)))

(define (parse-exprs sexprs)
  (let loop ((sexprs sexprs) (before-exprs '()) (after-exprs '()) (exprs '()))
    (cond
     ((null? sexprs) ;; done?
      (values (reverse (map normalize-before/after-expr before-exprs))
              (reverse (map normalize-before/after-expr after-exprs))
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

(define (read-exprs-from-string s)
  (with-input-from-string s
    (lambda () (port->sexp-list (current-input-port)))))

(define (read-exprs-from-file filename)
  (with-input-from-file filename
    (lambda () (port->sexpr-list (current-input-port)))))

(define (process-exprs exprs)
  ;; evaluate a list of expressions.
  (for-each (cut eval <> #f) exprs))

(define (process-line line exprs)
  (set! *current-line* line)
  (set! *fields* (string-split line fs))
  ;; also set *fields*, etc
  (process-exprs exprs))

(define (process-file port exprs)
  (let* ((data (read-data port))
         (lines (string-split data ls)))
    (for-each (cut process-line <> exprs) lines)))

;;; --- fields ---

(define (field n)
  (cond
   ((= n 0) *current-line*)
   ((> n 0) (list-ref *fields* (- n 1)))
   (else ...)))

;;; --- output ---

(define (out . args)
  (print-out ofs ols args))

(define (print-out ofs ols args)
  (let* ((args-as-strings (map ->string args))
         (s (string-join args-as-strings ofs)))
    (display s)
    (display ols)))
