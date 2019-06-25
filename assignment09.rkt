#lang sicp

;; Exercise 1.40: Define a procedure cubic that can be used together with the
;; newtons-method procedure in expressions of the form
;;     (newtons-method (cubic a b c) 1)
;; to approximate zeros of the cubic x^3 + ax^2 + bx + c.
(define (cubic a b c)
  (let ([cube (lambda (x) (* x x x))]
        [square (lambda (x) (* x x))])
    (lambda (x) (+ (cube x)
                   (* a (square x))
                   (* b x)
                   c))))

;; Exercise 1.41: Define a procedure double that takes a procedure of one
;; argument as argument and returns a procedure that applies the original
;; procedure twice. For example, if inc is a procedure that adds 1 to its
;; argument, then (double inc) should be a procedure that adds 2. What value is
;; returned by
;;     (((double (double double)) inc) 5)
(define (double f) (lambda (x) (f (f x))))
;; The value returned by the expression (((double (double double)) inc) 5)
;; is 21

;; Exercise 1.42: Let f and g be two one-argument functions. The composition f
;; after g is defined to be the function x → f(g(x)). Define a procedure
;; compose that implements composition. For example, if inc is a procedure
;; that adds 1 to its argument,
;;     ((compose square inc) 6)
;;     49
(define (compose f g) (lambda (x) (f (g x))))

;; Exercise 1.43: If f is a numerical function and n is a positive integer, then
;; we can form the nth repeated application of f , which is defined to be the
;; function whose value at x is f(f(...(f(x))...)). For example, if f
;; is the function x → x + 1, then the nth repeated application of f is the
;; function x → x + n. If f is the operation of squaring a number, then the nth
;; repeated application of f is the function that raises its argument to the
;; 2n-th power. Write a procedure that takes as inputs a procedure that computes
;; f and a positive integer n and returns the procedure that computes the nth
;; repeated application of f . Your procedure should be able to be used as
;; follows:
;;    ((repeated square 2) 5)
;;    625
;; Hint: You may find it convenient to use compose from Exercise 1.42.
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

;; Exercise 1.44: The idea of smoothing a function is an important concept in
;; signal processing. If f is a function and dx is some small number, then the
;; smoothed version of f is the function whose value at a point x is the average
;; of f(x − dx), f(x), and f(x + dx). Write a procedure smooth that takes as
;; input a procedure that computes f and returns a procedure that computes the
;; smoothed f. It is sometimes valuable to repeatedly smooth a function (that
;; is, smooth the smoothed function, and so on) to obtain the n-fold smoothed
;; function. Show  how to generate the n-fold smoothed function of any given
;; function using smooth and repeated from Ex. 1.43.
(define (smooth f)
  (let ([dx 0.00001]
        [average (lambda (x y z) (/ (+ x y z) 3))])
    (lambda (x) (average (f (- x dx))
                         (f x)
                         (f (+ x dx))))))

(define (n-fold-smooth f n)
  (repeated (smooth f) n))

;; Exercise 1.45: We saw in Section 1.3.3 that attempting to compute square
;; roots by naively finding a fixed point of y → x/y does not converge, and
;; that this can be fixed by average damping. The same method works for finding
;; cube roots as fixed points of the average-damped y → x/y^2. Unfortunately,
;; the process does not work for fourth roots—a single average damp is not
;; enough to make a fixed-point search for y → x/y^3 converge. On the other
;; hand, if we average damp twice (i.e., use the average damp of the average
;; damp of y → x/y^3) the fixed-point search does converge. Do some experiments
;; to determine how many average damps are required to compute nth roots as a
;; fixed-point search based upon repeated average damping of y → x/y^(n−1). Use
;; this to implement a simple procedure for computing nth roots using
;; fixed-point, average-damp, and the repeated procedure of Exercise
;; 1.43. Assume that any arithmetic operations you need are available as
;; primitives.
(define (average-damp f)
  (let ([average (lambda (x y) (/ (+ x y) 2))])
    (lambda (x)
      (average x (f x)))))

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

(define (nth-root x n)
  (fixed-point (repeated (average-damp (lambda (y) (/ x (expt y (- n 1)))))
                         n)
               1.0))

;; Exercise 1.46: Several of the numerical methods described in this chapter
;; are instances of an extremely general computational strategy known as
;; iterative improvement. Iterative improvement says that, to compute
;; something, we start with an initial guess for the answer, test if the guess
;; is good enough, and otherwise improve the guess and continue the process
;; using the improved guess as the new guess. Write a procedure
;; iterative-improve that takes two procedures as arguments: a method for
;; telling whether a guess is good enough and a method for improving a
;; guess. Iterative-improve should return as its value a procedure that takes a
;; guess as argument and keeps improving the guess until it is good
;; enough. Rewrite the sqrt procedure of Section 1.1.7 and the fixed-point
;; procedure of Section 1.3.3 in terms of iterative-improve.
(define (iterative-improve good-enough? improve)
  (define (f guess)
    (if (good-enough? guess)
        guess
        (f (improve guess))))
  f)

(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
(define (nah/sqrt x)
  ((iterative-improve (lambda (guess) (< (abs (- (square guess) x))
                                          0.0001))
                      (lambda (guess) (average guess (/ x guess)))) 1.0))
(define (nah/fixed-point f first-guess)
  ((iterative-improve (lambda (guess)))))
