;; s1.scm

;;; --- special variables ---

(define *fields* '()) ;; fields in the current line

(define fs #f)        ;; field separator; if undefined, use whitespace
(define ls "\n")      ;; line separator; if undefined, use newline
(define ofs " ")      ;; output file separator
(define ols "\n")     ;; output line separator

(define (read-data port)
  ;; read all data from the port, and return it as a string.
  (with-input-from-port port
    (lambda ()
      (port->string port))))

(define (split-data data)
  ;; split data using LS
  (string-split data ls))

(define (write-data port)
  ...)

(define (main args)
  (print "Hello world!")
  (print args)
  (print *argv*))

;; read data from stdin
;; write data to stdout

#|
proposed command line options:
-e expr     read code from expr
-f file     read code from file
(either -e or -f is required, or it will just pipe stdin to stdout)

"regular" args are assumed to be names of files with data to be read/processed.
|#