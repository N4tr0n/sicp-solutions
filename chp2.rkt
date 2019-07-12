#lang sicp
;; 2.1.3
;; Exercise 2.4: Here is an alternative procedural representation of pairs. For
;; this representation, verify that (car (cons x y)) yields x for any objects x
;; and y.
;;     (define (cons x y) (lambda (m) (m x y)))
;;     (define (car z) (z (lambda (p q) p)))
;; What is the corresponding definition of cdr? (Hint: To verify that this works,
;; make use of the substitution model of Section 1.1.5.)
(define (cons1 x y) (lambda (m) (m x y)))
(define (car1 z) (z (lambda (p q) p)))
(define (cdr1 z) (z (lambda (p q) q)))

;; (car1 (cons1 x y))
;; (car1 (lambda (m) (m x y)))
;; ((lambda (m) (m x y)) (lambda (p q) p))
;; ((lambda (p q) p) x y)
;; x

;; Exercise 2.5: Show that we can represent pairs of nonnegative integers using
;; only numbers and arithmetic operations if we represent the pair a and b as
;; the integer that is the product 2^a * 3^b. Give the corresponding definitions
;; of the procedures cons, car, and cdr.
(define (cons2 a b)
  (* (expt 2 a) (expt 3 b)))

(define (car2 z)
  (define (loop acc x)
    (if (= (remainder x 2) 0)
        (loop (+ acc 1) (/ x 2))
        acc))
  (loop 0 z))

(define (cdr2 z)
  (define (loop acc x)
    (if (= (remainder x 3) 0)
        (loop (+ acc 1) (/ x 3))
        acc))
  (loop 0 z))

;; Exercise 2.6: In case representing pairs as procedures wasn’t mind-boggling
;; enough, consider that, in a language that can manipulate procedures, we can
;; get by without numbers (at least insofar as nonnegative integers are
;; concerned) by implementing 0 and the operation of adding 1 as
;;    (define zero (lambda (f) (lambda (x) x)))
;;    (define (add-1 n)
;;      (lambda (f) (lambda (x) (f ((n f) x)))))
;; This representation is known as Church numerals, after its inventor, Alonzo
;; Church, the logician who invented the λ-calculus.
;; Define one and two directly (not in terms of zero and add-1). (Hint: Use
;; substitution to evaluate (add-1 zero)). Give a direct definition of the
;; addition procedure + (not in terms of repeated application of add-1).

;; (add-1 zero)
;; (add-1 (lambda (f) (lambda (x) x)))
;; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))


;; 2.1.2
(define (make-point x y)
  "x and y should be numbers"
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (average x y)
  (/ (+ x y) 2))

(define (midpoint-segment s)
  (let ((start (start-segment s))
        (end (end-segment s)))
    (cons (average (x-point start)
                   (x-point end))
          (average (y-point start)
                   (y-point end)))))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-size width height)
  (cons width height))

(define (width size)
  (car size))

(define (height size)
  (cdr size))

(define (rectangle origin size)
  (cons origin size))

(define (rectangle-origin rectangle)
  (car rectangle))

(define (rectangle-size rectangle)
  (cdr rectangle))

(define (area rectangle)
  (let ((size (rectangle-size rectangle)))
    (* (width size)
       (height size))))

(define (perimeter rectangle)
  (let ((size (rectangle-size rectangle)))
    (* 2 (+ (width size)
            (height size)))))
