#lang sicp

;; Library
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


;; Exercise 1.35: Show that the golden ratio φ (Section 1.2.2) is a
;; fixed point of the transformation x → 1 + 1/x, and use this fact to
;; compute φ by means of the fixed-point procedure.
(fixed-point (lambda (y) (+ 1 (/ 1 y)))
             1.0)


;; Exercise 1.36: Modify fixed-point so that it prints the sequence of
;; approximations it generates, using the newline and display primitives
;; shown in Exercise 1.22. Then find a solution to x^x = 1000 by finding a
;; fixed point of x → log(1000)/log(x). (Use Scheme’s primitive log
;; procedure, which computes natural logarithms.) Compare the number of
;; steps this takes with and without average damping. (Note that you cannot
;; start fixed-point with a guess of 1, as this would cause division by
;; log(1) = 0.)
(define (fixed-point-print f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point-print (lambda (x) (/ (log 1000) (log x)))
                   2.0)
;; 2.0
;; 9.965784284662087
;; 3.004472209841214
;; 6.279195757507157
;; 3.759850702401539
;; 5.215843784925895
;; 4.182207192401397
;; 4.8277650983445906
;; 4.387593384662677
;; 4.671250085763899
;; 4.481403616895052
;; 4.6053657460929
;; 4.5230849678718865
;; 4.577114682047341
;; 4.541382480151454
;; 4.564903245230833
;; 4.549372679303342
;; 4.559606491913287
;; 4.552853875788271
;; 4.557305529748263
;; 4.554369064436181
;; 4.556305311532999
;; 4.555028263573554
;; 4.555870396702851
;; 4.555315001192079
;; 4.5556812635433275
;; 4.555439715736846
;; 4.555599009998291
;; 4.555493957531389
;; 4.555563237292884
;; 4.555517548417651
;; 4.555547679306398
;; 4.555527808516254
;; 4.555540912917957
;; 4.555532270803653

;; Exercise 1.37:
;; a. An infinite continued fraction is an expression of the form As an
;; example, one can show that the infinite continued fraction expansion
;; with the N_i and the D_i all equal to 1 produces 1/φ, where φ is the
;; golden ratio (described in Section 1.2.2). One way to approximate an
;; infinite continued fraction is to truncate the expansion after a given
;; number of terms. Such a truncation -- a so-called k-term finite
;; continued fraction -- has the form shown in the book. Suppose that n and
;; d are procedures of one argument (the term index i) that return the N_i
;; and D_i of the terms of the continued fraction. Define a procedure
;; `cont-frac` such that evaluating `(cont-frac n d k)` computes the value
;; of the k-term finite continued fraction. Check your procedure by
;; approximating 1/φ using
;; (cont-frac (lambda (i) 1.0)
;;            (lambda (i) 1.0)
;;            k)
;; for successive values of k. How large must you make k in order to get an
;; approximation that is accurate to 4 decimal places?
(define (cont-frac-recur n d k)
  (define (recur i)
    (if (= i k)
        1
        (/ (n k)
           (+ (d k)
              (recur (+ i 1))))))
  (recur 1))
;; b. If your cont-frac procedure generates a recursive process, write one
;; that generates an iterative process. If it generates an iterative
;; process, write one that generates a recursive process.
(define (cont-frac-iter n d k)
  )

(define cont-frac cont-frac-iter)
;; Exercise 1.38: In 1737, the Swiss mathematician Leonhard Euler published
;; a memoir De Fractionibus Continuis, which included a continued fraction
;; expansion for e − 2, where e is the base of the natural logarithms. In
;; this fraction, the N_i are all 1, and the D_i are successively 1, 2, 1,
;; 1, 4, 1, 1, 6, 1, 1, 8, .... Write a program that uses your cont-frac
;; procedure from Exercise 1.37 to approximate e, based on Euler’s
;; expansion.
(cont-frac (lambda (i) 1.0)
           (lambda (i)
             (cond [(= i 1) 1]
                   [(= 1 2) 2]
                   i))
           10)

;; Exercise 1.39: A continued fraction representation of the tangent
;; function was published in 1770 by the German mathematician J.H. Lambert:
;; (see expression in the book) where x is in radians. Define a procedure
;; (tan-cf x k)
;; that computes an approximation to the tangent function based on
;; Lambert’s formula. k specifies the number of terms to compute, as in
;; Exercise 1.37.
(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                   x
                   (square x)))
             (lambda (i) (- (* 2 i) 1))
             k))
