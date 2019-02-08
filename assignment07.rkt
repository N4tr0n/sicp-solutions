#lang sicp

;; Exercise 1.29
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (simpson-next x)
    (+ x h))
  (* (/ h 3) (sum-recur f a simpson-next b)))


;; Exercise 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter  (next a) (+ (term a) result))))
  (iter a 0))

;; Exercise 1.31
;; a.
(define (product-recur term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recur term (next a) next b))))

;; b.
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (product-iter identity 1 inc n))

;; Exercise 1.32
;; a.
(define (accumulate-recur combiner null-value term a next b)
  (define (recur a)
    (if (> a b)
        null-value
        (combiner (term a)
                  (recur (next a)))))
  (recur a))

(define (sum-acc-recur term a next b) (accumulate-recur + 0 term a next b))
(define (product-acc-recur term a next b) (accumulate-recur * 1 term a next b))

;; b.
(define (accumulate-iter combiner null-value term a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x) (combiner (term x) result))))
  (iter a null-value))

(define (sum-acc-iter term a next b) (accumulate-iter + 0 term a next b))
(define (product-acc-iter term a next b) (accumulate-iter * 1 term a next b))

;; Exercise 1.33
(define (filtered-accumulate combiner predicate null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (if (predicate a)
            (iter (next a) (combiner (term a) result))
            (iter (next a) result))))
  (iter a null-value))
;; a.
(define (sum-of-squares-of-primes a b)
  (filtered-accumulate + prime? 0 square a inc b))

;; b.
(define (product-relative-primes n)
  (define (relatively-prime? a) (= (gcd a n) 1))
  (filtered-accumulate * relatively-prime? 1 identity 1 inc n))

;; Library
(define (identity x) x)
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (sum-of-squares x y) (+ (square x) (square y)))
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
