;; core.scm

(use srfi-13)

;;; --- special variables ---

(define *debug* #f)

(define *current-line* #f) ;; current line being processed

(define *fields* '()) ;; fields in the current line

(define fs #/[ \n\t\r]+/) ;; field separator; if undefined, use whitespace

(define ls "\n")      ;; line separator; if undefined, use newline
(define ofs " ")      ;; output file separator
(define ols "\n")     ;; output line separator

(define nf 0) ;; number of fields; set for each line
(define nl 0) ;; number of lines in file; set when file is read

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
              (reverse (map replace-$-syntax exprs))))

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
  (set! nf (length *fields*))
  (when *debug*
    (format (standard-error-port) "~s~%" *fields*))
  ;; also set *fields*, etc
  (process-exprs exprs))

(define (process-file port exprs)
  (let* ((data (read-data port))
         (lines (string-split data ls)))
    (set! nl (length lines))
    (for-each (cut process-line <> exprs) lines)))

;;; --- $ shorthand ---

;; In Chicken I could define a reader macro to have $N mean (field N);
;; this does not seem to work in Gauche, so we replace the appropriate
;; symbols in expressions by hand. Kludgy, but it'll do for now.
;; Because of this restriction, however, we only support ${number} and $nf.

(define (replace-$-syntax expr)
  (search-and-replace expr matches-$-syntax? convert-$-syntax))

(define (matches-$-syntax? sym)
  (if (symbol? sym)
      (let ((s (symbol->string sym)))
        (string-prefix? "$" s))
      #f))

(define (convert-$-syntax sym)
  (let ((s (symbol->string sym)))
    (if (equal? s "$nf")
        '(field nf)
        (list 'field (string->number (string-copy s 1))))))

;;; --- fields ---

(define (safe-field-get n)
  (if (> n (length *fields*))
      ""
      (list-ref *fields* (- n 1))))

(define (field n)
  (cond
   ((= n 0) *current-line*)
   ((> n 0) (safe-field-get n))
   (else (safe-field-get (+ (+ (length *fields*) 1) n)))))

;;; --- macros ---

(define-syntax def
  (syntax-rules ()
    ((def) #f)

    ((def (name value) rest ...)
     (begin
       (define name value)
       (def rest ...)))

    ((def name rest ...)
     (begin
       (define name 0)
       (def rest ...)))))

(define (?false? x)
  (not (not (member x '(0 "" () 0.0 #f)))))

(define-syntax ?
  (syntax-rules ()
    ((? cond true-expr false-expr)
     (if (?false? cond)
         false-expr
         true-expr))
    ((? cond true-expr)
     (? cond true-expr #f))))

(define (?out x)
  (? x (out x)))

;;; --- regular expressions ---



;;; --- output ---

(define (out . args)
  (print-out ofs ols args))

(define (print-out ofs ols args)
  (let* ((args-as-strings (map ->string args))
         (s (string-join args-as-strings ofs)))
    (display s)
    (display ols)))
