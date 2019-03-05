;; Exercise 1.40: Define a procedure cubic that can be used together with the
;; newtons-method procedure in expressions of the form
;;     (newtons-method (cubic a b c) 1)
;; to approximate zeros of the cubic x^3 + ax^2 + bx + c.

;; Exercise 1.41: Define a procedure double that takes
;; a procedure of one argument as argument and returns a procedure that
;; applies the original procedure twice. For example, if inc is a procedure
;; that adds 1 to its argument, then (double inc) should be a procedure that
;; adds 2. What value is returned by
;;     (((double (double double)) inc) 5)

;; Exercise 1.42: Let f and g be two one-argument functions. The composition f
;; after g is defined to be the function x → f(g(x)). Define a procedure
;; compose that implements composition. For example, if inc is a procedure
;; that adds 1 to its argument,
;;     ((compose square inc) 6)
;;     49
