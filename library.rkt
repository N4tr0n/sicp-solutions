#lang sicp

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (average x y)
  (/ (+ x y) 2))

(define (identity x) x)

(define (cube x) (* x x x))

(define (sum-recur term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum-recur term (next a) next b))))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (define (next x)
    (cond [(= 2 x) 3]
          [else (+ x 2)]))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n) (= n (smallest-divisor n)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(#%provide square
           sum-of-squares
           average
           identity
           cube
           sum-recur
           smallest-divisor
           find-divisor
           divides?
           prime?
           gcd
           tolerance
           fixed-point)
