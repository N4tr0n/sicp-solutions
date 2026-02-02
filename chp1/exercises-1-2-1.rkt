#lang sicp

;; Exercise 1.9: Each of the following two procedures defines a method for
;; adding two positive integers in terms of the procedures inc, which increments
;; its argument by 1, and dec, which decrements its argument by 1.
(define (nh-plus1 a b)
  (if (= a 0)
      b
      (inc (nh-plus1 (dec a) b))))
(nh-plus1 5 6)
(define (nh-plus2 a b)
  (if (= a 0)
      b
      (nh-plus2 (dec a) (inc b))))
;; Solution:
;; (inc (+ (dec 4) 5))
;; (inc (+ 3 5))
;; (inc (inc (+ (dec 3) 5)))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ (dec 2) 5))))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ (dec 1) 5)))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9
;; (+ (dec 4) (inc 5))
;; (+ 3 6)
;; (+ (dec 3) (inc 6))
;; (+ 2 7)
;; (+ (dec 2) (inc 7))
;; (+ 1 8)
;; (+ (dec 1) (inc 8))
;; (+ 0 9)
;; 9
;;    The first process is recursive while the second process is iterative.

;; Exercise 1.10: The following procedure computes a mathematical function
;; called Ackermann's function.
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
;; What are the values of the following expressions?
(A 1 10) ;; 1024
(A 2 4)  ;; 65536
(A 3 3)  ;; 65536
;; Consider the following procedures, where =A= is the procedure defined above:
(define (f n) (A 0 n)) ;; = 2n
(define (g n) (A 1 n)) ;; = 2^n
(define (h n) (A 2 n)) ;; = ^n2 (the nth tetration of 2)
(define (k n) (* 5 n n)) ;; = 5n^2
;; Give concise mathematical definitions for the functions computed by the
;; procedures f, g, and h for positive integer values of n. For example, (k n)
;; computes 5n^2.
;; Solution:
;; (A 1 10)
;; (A 0 (A 1 9))
;; (A 0 (A 0 (A 1 8)))
;; (A 0 (A 0 (A 0 (A 1 7))))
;; (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 32)))))
;; (A 0 (A 0 (A 0 (A 0 64))))
;; (A 0 (A 0 (A 0 128)))
;; (A 0 (A 0 256))
;; (A 0 512)
;; 1024
;;
;; (A 2 4)
;; (A 1 (A 2 3))
;; (A 1 (A 1 (A 2 2)))
;; (A 1 (A 1 (A 1 (A 2 1))))
;; (A 1 (A 1 (A 1 2)))
;; (A 1 (A 1 (A 0 (A 1 1))))
;; (A 1 (A 1 (A 0 2)))
;; (A 1 (A 1 4))
;; (A 1 (A 0 (A 1 3)))
;; (A 1 (A 0 (A 0 (A 1 2))))
;; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
;; (A 1 (A 0 (A 0 (A 0 2))))
;; (A 1 (A 0 (A 0 4)))
;; (A 1 (A 0 8))
;; (A 1 16)
;; (A 0 (A 1 15))
;; (A 0 (A 0 (A 1 14)))
;; (A 0 (A 0 (A 0 (A 1 13))))
;; (A 0 (A 0 (A 0 (A 0 (A 1 12)))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 11))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 10)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1024))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1024))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 2048)))))
;; (A 0 (A 0 (A 0 (A 0 4096))))
;; (A 0 (A 0 (A 0 8192)))
;; (A 0 (A 0 16384))
;; (A 0 32768)
;; 65536
;;
;; (A 3 3)
;; (A 2 (A 3 2))
;; (A 2 (A 2 (A 3 1)))
;; (A 2 (A 2 2))
;; (A 2 (A 1 (A 2 1)))
;; (A 2 (A 1 2))
;; (A 2 4)
;; 65536
