#lang sicp

;; Exercise 1.11: A function f is defined by the rule that
;;        /
;;        | n if n < 3
;; f(n) = <
;;        | f(n-1) + 2f(n-2) + 3f(n-3) if n >= 3
;;        \
;; Write a procedure that computes f by means of a recursive procedure. Write a
;; procedure that computes f by means of an iterative procedure
;;  Solution:

(define (f-recur n)
  (if (< n 3)
      n
      (+ (f-recur (- n 1))
         (* 2 (f-recur (- n 2)))
         (* 3 (f-recur (- n 3))))))
(define (f n)
  (define (iter count a b c)
    (if (>= count n)
        a
        (iter (+ count 1)
              b
              c
              (+ (* 3 a)
                 (* 2 b)
                 c))))
  (iter 0 0 1 2))

;; * Exercise 1.12
;;   The following pattern of numbers is called Pascal's triangle.
;;     1
;;     1 1
;;     1 2 1
;;     1 3 3 1
;;     1 4 6 4 1
;;        ...
;; The numbers at the edge of the triangle are all 1, and each number inside the
;; triangle is the sum of the two numbers above it. Write a procedure that
;; computes elements of Pascal's triangle by means of a recursive process.
;; Solution:
(define (pascal row column)
  (cond ((or (< column 0)
             (> column row))
         0)
        ((or (= row 0)
             (= column 0)
             (= row column))
         1)
        (else
         (+ (pascal (- row 1)
                    (- column 1))
            (pascal (- row 1)
                    column)))))

;; Exercise 1.13: Prove that Fib(n) is the closest integer to (phi^n)/sqrt(5),
;; where phi = (1 + sqrt(5))/2. Hint: Let psi = (1 - sqrt(5))/2. Use induction
;; and the definition of the Fibonacci numbers to prove that
;; Fib(n) = (phi^n - psi^n)/sqrt(5).
;; Solution: This solution is incomplete. The formula for the nth fibonacci
;; number is
;;          /
;;          | 0 if n = 0
;; Fib(n) = < 1 if n = 1
;;          | Fib(n-1) + Fib(n-2) otherwise
;;          \
;; Setting n = 0, we have [phi^0] / sqrt(5) = 1/sqrt(5). Since sqrt(5) > 2, we
;; must have that 1/sqrt(5) < 1/2 and so is closer to zero than it is to
;; one. Thus the claim holds for the case n = 0. Similarly, for n = 1 we have
;; [phi^1] / sqrt(5) ~ 0.7236 which is closer to one than it is to zero as
;; needed. For the inductive step, we assume that Fib(k) is the closest integer
;; to (phi^k)/sqrt(5) and show that Fib(k+1) is the closest integer to
;; [phi^(k+1)] / sqrt(5).
