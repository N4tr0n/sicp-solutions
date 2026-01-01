#lang sicp

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (average x y)
  (/ (+ x y) 2))

(#%provide square
           sum-of-squares
           average)
