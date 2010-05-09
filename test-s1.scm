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

(test-end)
