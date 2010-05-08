;; test-s1.scm

(use gauche.test)
(load "./core.scm")

(test-start "dummy")
(test* "1+1" 2 (+ 1 1))
(test-end)

            