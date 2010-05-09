;; tools.scm

(define (->string x)
  (format "~a" x))

(define (search-and-replace expr match? replace-proc)
  (if (list? expr)
      (map (lambda (e) (search-and-replace e match? replace-proc))
           expr)
      (if (match? expr)
          (replace-proc expr)
          expr)))

