#lang sicp

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
