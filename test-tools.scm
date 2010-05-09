;; test-tools.scm

(use gauche.test)
(load "./tools")

(test-start "tools")

(test-section "search-and-replace")

(let ((expr1 '(a b c)))
  (test* "flat list" '(a 4 c)
         (search-and-replace
          expr1
          (lambda (x) (equal? x 'b))
          (lambda (x) 4))))

(let ((expr1 '(* (+ a b) (- a b) c)))
  (test* "nested list" '(* (+ z b) (- z b) c)
         (search-and-replace
          expr1
          (lambda (x) (and (symbol? x) (equal? x 'a)))
          (lambda (x) 'z))))

(test-end)
