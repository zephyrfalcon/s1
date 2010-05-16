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

;; read the complete contents of a file, and return it as one big string.
;; (the more straightforward form, using with-input-from-file and port->string,
;; does not seem to work.)
(define (read-all-from-file filename :key (blocksize 1024))
  (let ((p (open-input-file filename)))
    (let ((data (read-all-from-port p :blocksize blocksize)))
      (close-input-port p)
      data)))

(define (read-all-from-port port :key (blocksize 1024))
  (let loop ((chunks '()))
    (let ((chunk (read-block blocksize port)))
      (if (eof-object? chunk)
          ;; remove any "weird" characters
          (string-incomplete->complete (string-join (reverse chunks) "") :omit)
          (loop (cons chunk chunks))))))
