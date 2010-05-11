;; test-s1.scm

(use gauche.test)
(load "./core.scm")

(test-start "s1")

(test-section "parse-exprs")
(let ((test-exprs
       '((BEFORE (init this) (do that))
         (print (field 1))
         (AFTER (print total) (say bye)))))
  (receive (b a e) (parse-exprs test-exprs)
    (test* "after" '((begin (print total) (say bye))) a)
    (test* "before" '((begin (init this) (do that))) b)
    (test* "exprs" '((print (field 1))) e)))

(test-section "$ replacement")
(let ((expr '(when something (out $4))))
  (test* "#1" '(when something (out (field 4)))
         (replace-$-syntax expr)))

(test-section "? macro")
(test* "0 is false?" #t (?false? 0))
(test* "empty string is false?" #t (?false? ""))
(test* "\"0\" is false?" #f (?false? "0"))

(test* "? #1" 'bar (? "" 'foo 'bar))

(test-end)
