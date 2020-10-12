;; 1.1.6
;; Exercise 1.1: Below is a sequence of expressions. What is the result printed
;; by the interpreter in response to each expression? Assume that the sequence
;; is to be evaluated in the order in which it is presented.
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

;; Exercise 1.2: Translate the following expression into prefix form
;; (5 + 4 + (2 - (3 - (6 + 4/5)))) / (3(6 - 2)(2 - 7))
(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7))) ;; -37/150

;; Exercise 1.3: Define a procedure that takes three numbers as arguments and
;; returns the sum of the squares of the two larger numbers.
(define (sum-of-squares-of-larger x y z)
  (cond ((and (>= x z) (>= y z))
         (sum-of-squares x y))
        ((and (>= x y) (>= z y))
         (sum-of-squares x z))
        (else (sum-of-squares y z))))

;; Exercise 1.4: Observe that our model of evaluation allows for combinations
;; whose operators are compound expressions. Use this observation to describe
;; the behavior of the following procedure:
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
;; Solution: This procedure uses the sign of b to decide whether to add b to a
;; or subtract b from a. If b is greater than zero then b is added if b is less
;; than zero then b is subtracted essentially resulting in adding the absolute
;; value of b to a.

;; Exercise 1.5: Ben Bitdiddle has invented a test to determine whether the
;; interpreter he is faced with is using applicative-order evaluation or
;; normal-order evaluation. He defines the following two procedures:
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
;; Then he evaluates the expression
;; (test 0 (p))
;; What behavior will Ben observe with an interpreter that uses
;; applicative-order evaluation? What behavior will he observe with an
;; interpreter that uses normal-order evaluation? Explain your answer. (Assume
;; that the evaluation rule for the special form if is the same whether the
;; interpreter is using normal or applicative order: The predicate expression is
;; evaluated first, and the result determines whether to evaluate the consequent
;; or the alternative expression.)
;; Solution: Using applicative-order evaluation, the expression
;; (test 0 p)
;; will be evaluated by first evaluating the arguments. Thus, evaluating p leads
;; to an infinite recursion and the evaluation will never complete. Using
;; normal-order evaluation, we will first expand the expression then
;; reduce. When this happens, the if form's condition will evaluate to true and
;; the result of the expression will be zero with no attempt to evaluate p.

;; Exercise 1.6: Alyssa P. Hacker doesn't see why if needs to be provided as a
;; special form. "Why can't I just define it as an ordinary procedure in terms
;; of cond?" she asks. Alyssa's friend Eva Lu Ator claims this can indeed be
;; done, and she defines a new version of if:
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
;; Eva demonstrates the program for Alyssa:
(new-if (= 2 3) 0 5) ;; 5
(new-if (= 1 1) 0 5) ;; 0
;; Delighted, Alyssa uses =new-if= to rewrite the square-root program:
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))
;; What happens when Alyssa attempts to use this to compute square roots?
;; Explain.
;; Solution: Since new-if is a procedure and must evaluate all of its arguments,
;; sqrt-iter will loop forever. This happens because sqrt-iter must be evaluated
;; before new-if can be applied thus calling new-if again which will try to
;; evaluate sqrt-iter again and so on.

;; Exercise 1.7: The good-enough? test used in computing square roots will not
;; be very effective for finding the square roots of very small numbers. Also,
;; in real computers, arithmetic operations are almost always performed with
;; limited precision. This makes our test inadequate for very large
;; numbers. Explain these statements, with examples showing how the test fails
;; for small and large numbers. An alternative strategy for implementing
;; good-enough? is to watch how guess changes from one iteration to the next and
;; to stop when the change is a very small fraction of the guess. Design a
;; square-root procedure that uses this kind of end test. Does this work better
;; for small and large numbers?
;; Solution: For very small numbers we have a very large relative error. For
;; instance, in computing (sqrt 0.001) the difference between the good-enough?
;; result is 70% different than the original argument. For very large numbers
;; the machine precision is unable to represent small differences between large
;; numbers.
(define (good-enough? previous-guess next-guess)
  (< (abs (- previous-guess next-guess)) (* 0.001 next-guess)))
(define (sqrt-iter first-guess second-guess x)
  (if (good-enough? first-guess second-guess)
      second-guess
      (sqrt-iter second-guess (improve second-guess x) x)))
(define (sqrt x)
  (sqrt-iter x 1.0 x))

;; Exercise 1.8: Newton's method for cube roots is based on the fact that if y
;; is an approximation to the cube root of x, then a better approximation is
;; given by the value (x/y^2 + 2y) / 3.
;; Use this formula to implement a cube-root procedure analogous to the
;; square-root procedure.
;; Solution:
(define (square x)
  (* x x))
(define (good-enough? previous-guess next-guess)
  (< (abs (- previous-guess next-guess)) (* 0.001 next-guess)))
(define (improve guess x)
  (/ (+ (/ x (square y)) (* y 2.0)) 3.0))
(define (cbrt-iter first-guess second-guess x)
  (if (good-enough? first-guess second-guess)
      second-guess
      (cbrt-iter second-guess (improve second-guess x) x)))
(define (cube-root x)
  (cbrt-iter x 1.0 x))

;; Exercise 1.9: Each of the following two procedures defines a method for
;; adding two positive integers in terms of the procedures inc, which increments
;; its argument by 1, and dec, which decrements its argument by 1.
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))
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

;; Exercise 1.11: A function f is defined by the rule that
;;        ( n if n < 3
;; f(n) = (
;;        ( f(n-1) + 2f(n-2) + 3f(n-3) if n >= 3
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
;;          ( 0 if n = 0
;; Fib(n) = ( 1 if n = 1
;;          ( Fib(n-1) + Fib(n-2) otherwise
;; Setting n = 0, we have [phi^0] / sqrt(5) = 1/sqrt(5). Since sqrt(5) > 2, we
;; must have that 1/sqrt(5) < 1/2 and so is closer to zero than it is to
;; one. Thus the claim holds for the case n = 0. Similarly, for n = 1 we have
;; [phi^1] / sqrt(5) ~ 0.7236 which is closer to one than it is to zero as
;; needed. For the inductive step, we assume that Fib(k) is the closest integer
;; to (phi^k)/sqrt(5) and show that Fib(k+1) is the closest integer to
;; [phi^(k+1)] / sqrt(5).

;; These solutions are incomplete.
;; Exercise 1.14
;; Draw the tree illustrating the process generated by the =count-change=
;; procedure of Section 1.2.2 in making change for 11 cents. What are the orders
;; of the space and number of steps used by this process as the amount to be
;; changed increases?
;; Solution: This is the tree illustrating the process generated by
;; count-change:
;; (count-change 11)
;; |
;; (cc 11 5)__
;; |          \
;; (cc 11 4)   (cc -39 5)
;; |       \___
;; |           \
;; (cc 11 3)   (cc -14 4)
;; |       \_______________________________________________________
;; |                                                               \
;; (cc 11 2)                                                      (cc 1 3)
;; |       \_________________________                              |     \__
;; |                                 \                             |        \
;; (cc 11 1)                        (cc 6 2)                      (cc 1 2) (cc -9 2)
;; |       \___                      |     \__                     |     \__
;; |           \                     |        \                    |        \
;; (cc 11 0)   (cc 10 1)            (cc 6 1) (cc 1 2)             (cc 1 1) (cc -4 2)
;;          __/ |                 __/ |       |     \__            |     \__
;;         /    |                /    |       |        \           |        \
;; (cc 10 0)   (cc 9 1)  (cc 6 0)   (cc 5 1) (cc 1 1) (cc -4 2)   (cc 1 0) (cc 0 1)
;;          __/ |                 __/ |       |     \__
;;         /    |                /    |       |        \
;; (cc 9 0)    (cc 8 1)  (cc 5 0)   (cc 4 1) (cc 1 0) (cc 0 1)
;;          __/ |                 __/ |
;;         /    |                /    |
;; (cc 8 0)    (cc 7 1)  (cc 4 0)   (cc 3 1)
;;          __/ |                 __/ |
;;         /    |                /    |
;; (cc 7 0)    (cc 6 1)  (cc 3 0)   (cc 2 1)
;;          __/ |                 __/ |
;;         /    |                /    |
;; (cc 6 0)    (cc 5 1)  (cc 2 0)   (cc 1 1)
;;          __/ |                 __/ |
;;         /    |                /    |
;; (cc 5 0)    (cc 4 1)  (cc 1 0)   (cc 0 1)
;;          __/ |
;;         /    |
;; (cc 4 0)    (cc 3 1)
;;          __/ |
;;         /    |
;; (cc 3 0)    (cc 2 1)
;;          __/ |
;;         /    |
;; (cc 2 0)    (cc 1 1)
;;          __/ |
;;         /    |
;; (cc 1 0)    (cc 0 1)
;; Let a be the amount to be changed, then the space complexity is Theta(a)
;; while the time complexity is Theta(a^5).

;; Exercise 1.15
;; The sine of an angle (specified in radians) can be computed by making use of
;; the approximation sin(x) ~ x if x is sufficiently small, and the
;; trigonometric identity sin(x) = 3*sin(x/3) - 4*sin^3(x/3) to reduce the size
;; of the argument of sin. (For purposes of this exercise an angle is considered
;; "sufficiently small" if its magnitude is not greater than 0.1 radians.) These
;; ideas are incorporated in the following procedures:
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))
;; a. How many times is the procedure p applied when (sine 12.15) is
;;    evaluated?
;;    p is applied 5 times.
;; b. What is the order of growth in space and number of steps (as a function of
;;    a used by the process generated by the sine procedure when (sine a) is
;;    evaluated?

;; Exercise 1.16
;; Design a procedure that evolves an iterative exponentiation process that uses
;; successive squaring and uses a logarithmic number of steps, as does
;; fast-expt. (Hint: Using the observation that (b^(n/2))^2 = (b^2)^(n/2), keep,
;; along with the exponent n and the base b, an additional state variable a, and
;; define the state transformation in such a way that the product ab^n is
;; unchanged from state to state. At the beginning of the process a is taken to
;; be 1, and the answer is given by the value of a at the end of the process. In
;; general, the technique of defining an invariant quantity that remains
;; unchanged from state to state is a powerful way to think about the design of
;; iterative algorithms.)
;; Solution:
(define (nh-fast-expt base exponent)
  (define (square x) (* x x))
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 base exponent))

;; Exercise 1.17: The exponentiation algorithms in this section are based on
;; performing exponentiation by means of repeated multiplication. In a similar
;; way, one can perform integer multiplication by means of repeated
;; addition. The following multiplication procedure (in which it is assumed that
;; our language can only add, not multiply) is analogous to the expt procedure:
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))
;; This algorithm takes a number of steps that is linear in b. Now suppose we
;; include, together with addition, operations double, which doubles an integer,
;; and halve, which divides an (even) integer by 2. Using these, design a
;; multiplication procedure analogous to fast-expt that uses a logarithmic
;; number of steps.
;; Solution:
(define (fast-mult a b)
  (define (double x) (+ x x))
  (define (halve x) (if (even? x) (/ x 2) x))
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mult a (halve b))))
        (else (+ a (fast-mult a (- b 1))))))

;; Exercise 1.18: Using the results of Exercise 1.16 and Exercise 1.17, devise a
;; procedure that generates an iterative process for multiplying two integers in
;; terms of adding, doubling, and halving and uses a logarithmic number of
;; steps.
;; Solution:
(define (nh-fast-mult a b)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (define (iter product a b)
    (cond ((= b 0) product)
          ((even? b) (iter product (double a) (halve b)))
          (else (iter (+ product a) a (- b 1)))))
  (iter 0 a b))

;; Exercise 1.19: There is a clever algorithm for computing the Fibonacci
;; numbers in a logarithmic number of steps. Recall the transformation of the
;; state variables a and b in the fib-iter process of Section 1.2.2: a -> a + b
;; and b -> a. Call this transformation T, and observe that applying T over and
;; over again n times, starting with 1 and 0, produces the pair Fib(n + 1) and
;; Fib(n). In other words, the Fibonacci numbers are produced by applying T^n,
;; the nth power of the transformation T, starting with the pair (1, 0). Now
;; consider T to be the special case of p = 0 and q = 1 in a family of
;; transformations T_{pq}, T_{pq} transforms the pair (a, b) according to
;; a -> bq + aq + ap and b -> bp + aq. Show that if we apply such a
;; transformation T_{pq} twice, the effect is the same as using a single
;; transformation T_{p'q'} of the same form, and compute p' and q' in terms of p
;; and q. This gives us an explicit way to square these transformations, and
;; thus we can compute T_n using successive squaring, as in the fast-expt
;; procedure. Put this all together to complete the following procedure, which
;; runs in a logarithmic number of steps:
(define (fib n) (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   -1 ; compute p'
                   -1 ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
;; Solution:
(define (fib n) (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (define (square x) (* x x))
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (square q) (* 2 p q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;; Exercise 1.20: The process that a procedure generates is of course dependent
;; on the rules used by the interpreter. As an example, consider the iterative
;; gcd procedure given above. Suppose we were to interpret this procedure using
;; normal-order evaluation, as discussed in Section 1.1.5. (The
;; normal-order-evaluation rule for if is described in Exercise 1.5.) Using the
;; substitution method (for normal order), illustrate the process generated in
;; evaluating (gcd 206 40) and indicate the remainder operations that are
;; actually performed. How many remainder operations are actually performed in
;; the normal-order evaluation of (gcd 206 40)? In the applicative-order
;; evaluation?
;; Solution: Evaluating (gcd 206 40) using normal order we get
;; (gcd 206 40)
;;
;; (if (= 40 0)
;;     206
;;     (gcd 40 (remainder 206 40)))
;;
;; expand gcd
;; (if (= (remainder 206 40) 0)
;;     40
;;     (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
;; (if (= 6 0) ; 1
;;     40
;;     (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
;;
;; expand gcd
;; (if (= (remainder 40 (remainder 206 40)) 0)
;;     (remainder 206 40)
;;     (gcd (remainder 40 (remainder 206 40))
;;          (remainder (remainder 206 40)
;;                     (remainder 40 (remainder 206 40)))))
;; (if (= (remainder 40 6) 0) ; 2
;;     (remainder 206 40)
;;     (gcd (remainder 40 (remainder 206 40))
;;          (remainder (remainder 206 40)
;;                     (remainder 40 (remainder 206 40)))))
;; (if (= 4 0) ; 3
;;     (remainder 206 40)
;;     (gcd (remainder 40 (remainder 206 40))
;;          (remainder (remainder 206 40)
;;                     (remainder 40 (remainder 206 40)))))
;; expand gcd
;; (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0)
;;     (remainder 40 (remainder 206 40))
;;     (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;;          (remainder (remainder 40 (remainder 206 40))
;;                     (remainder (remainder 206 40)
;;                                (remainder 40 (remainder 206 40))))))
;; (if (= (remainder 6 (remainder 40 6)) 0) ; 4 and 5
;;     (remainder 40 (remainder 206 40))
;;     (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;;          (remainder (remainder 40 (remainder 206 40))
;;                     (remainder (remainder 206 40)
;;                                (remainder 40 (remainder 206 40))))))
;; Applicative order
;; (gcd 206 40)
;; (gcd 40 (remainder 206 40))
;; (gcd 40 6)
;; (gcd 6 (remainder 40 6))
;; (gcd 6 4)
;; (gcd 4 (remainder 6 4))
;; (gcd 4 2)
;; (gcd 2 (remainder 4 2))
;; (gcd 2 0)
;; 2
;; So, remainder is evaluated 4 times using applicative order and a lot more
;; times (18) using normal order.

;; Exercise 1.21: Use the smallest-divisor procedure to find the smallest
;; divisor of each of the following numbers: 199, 1999, 19999.
;; Solution: This is the smallest-divisor procedure:
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (square x) (* x x))
;; Applying ~smallest-divisor~ to each of 199, 1999, and 19999 yields
(smallest-divisor 199)   ; = 199
(smallest-divisor 1999)  ; = 1999
(smallest-divisor 19999) ; = 7

;; Exercise 1.22
;; Most Lisp implementations include a primitive called runtime that returns an
;; integer that specifies the amount of time the system has been running
;; (measured, for example, in microseconds). The following timed-prime-test
;; procedure, when called with an integer n, prints n and checks to see if n is
;; prime. If n is prime, the procedure prints three asterisks followed by the
;; amount of time used in performing the test.
(define (prime? n)
  (= n (smallest-divisor n)))
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime n elapsed-time)
  (display " *** ")
  (display elapsed-time))
;; Using this procedure, write a procedure search-for-primes that checks the
;; primality of consecutive odd integers in a specified range. Use your
;; procedure to find the three smallest primes larger than 1000; larger than
;; 10,000; larger than 100,000; larger than 1,000,000. Note the time needed to
;; test each  prime. Since the testing algorithm has order of growth of
;; theta(sqrt(n)), you should expect that testing for primes around 10,000
;; should take about sqrt(10) times as long as testing for primes around
;; 1000. Do your timing data bear this out? How well do the data for 100,000 and
;; 1,000,000 support the theta(sqrt(n)) prediction? Is your result compatible
;; with the notion that programs on your machine run in time proportional to the
;; number of steps required for the computation?
;; Solution:
(define (search-for-primes start end)
  (cond ((> start end) (display "Done\n"))
        ((even? start) (search-for-primes (+ start 1) end))
        (else (begin
                (timed-prime-test startp)
                (search-for-primes (+ start 1) end)))))
;; | prime number | time (in microseconds?) |
;; |--------------+-------------------------|
;; |         1009 |                       1 |
;; |         1013 |                       1 |
;; |         1019 |                       1 |
;; |        10007 |                       2 |
;; |        10009 |                       1 |
;; |        10037 |                       1 |
;; |       100003 |                       5 |
;; |       100019 |                       5 |
;; |       100043 |                       5 |
;; |      1000003 |                      15 |
;; |      1000033 |                      15 |
;; |      1000037 |                      15 |
;; |     10000019 |                      48 |
;; |     10000079 |                      48 |
;; |     10000103 |                      47 |

;; Exercise 1.23: The smallest-divisor procedure shown at the start of this
;; section does lots of needless testing: After it checks to see if the number
;; is divisible by 2 there is no point in checking to see if it is divisible by
;; any larger even numbers. This suggests that the values used for test-divisor
;; should not be 2, 3, 4, 5, 6, ..., but rather 2, 3, 5, 7, 9, .... To implement
;; this change, define a procedure next that returns 3 if its input is equal to
;; 2 and otherwise returns its input plus 2. Modify the smallest-divisor
;; procedure to use (next test-divisor) instead of (+ test-divisor 1). With
;; timed-prime-test incorporating this modified version of smallest-divisor, run
;; the test for each of the 12 primes found in Exercise 1.22. Since this
;; modification halves the number of test steps, you should expect it to run
;; about twice as fast. Is this expectation confirmed? If not, what is the
;; observed ratio of the speeds of the two algorithms, and how do you explain
;; the fact that it is different from 2?
;; Solution:
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (define (next x)
    (cond [(= 2 x) 3]
          [else (+ x 2)]))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
;; This is the output from running this version of the program on the primes in
;; the previous exercise:
;; |    prime | time |
;; |----------+------|
;; |     1009 |    5 |
;; |     1013 |    1 |
;; |     1019 |    1 |
;; |    10007 |    4 |
;; |    10009 |    4 |
;; |    10037 |    4 |
;; |   100003 |    6 |
;; |   100019 |   23 |
;; |   100043 |    6 |
;; |  1000003 |   16 |
;; |  1000033 |   16 |
;; |  1000037 |   16 |
;; | 10000019 |   61 |
;; | 10000079 |   50 |
;; | 10000103 |   50 |

;; Exercise 1.24: Modify the timed-prime-test procedure of Exercise 1.22 to use
;; fast-prime? (the Fermat method), and test each of the 12 primes you found in
;; that exercise. Since the Fermat test has theta(log(n)) growth, how would you
;; expect the time to test primes near 1,000,000 to compare with the time needed
;; to test primes near 1000? Do your data bear this out? Can you explain any
;; discrepancy you find?
;; Solution:
;; |    prime | time |
;; |----------+------|
;; |     1009 |  276 |
;; |     1013 |    4 |
;; |     1019 |    5 |
;; |    10007 |    5 |
;; |    10009 |    5 |
;; |    10037 |    5 |
;; |   100003 |   16 |
;; |   100019 |    6 |
;; |   100043 |    5 |
;; |  1000003 |    6 |
;; |  1000033 |    6 |
;; |  1000037 |    6 |
;; | 10000019 |    7 |
;; | 10000079 |    8 |
;; | 10000103 |    7 |

;; Exercise 1.25: Alyssa P. Hacker complains that we went to a lot of extra work
;; in writing expmod. After all, she says, since we already know how to compute
;; exponentials, we could have simply written
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))
;; Is she correct? Would this procedure serve as well for our fast prime tester?
;; Explain.

;; Exercise 1.26: Louis Reasoner is having great difficulty doing Exercise
;; 1.24. His fast-prime? test seems to run more slowly than his prime?
;; test. Louis calls his friend Eva Lu Ator over to help. When they examine
;; Louis’s code, they find that he has rewritten the expmod procedure to use an
;; explicit multiplication, rather than calling square:
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base
                       (expmod base (- exp 1) m))
                    m))))
;; "I don't see what difference that could make," says Louis. "I do." says
;; Eva. "By writing the procedure like that, you have transformed the
;; theta(log(n)) process into a theta(n) process." Explain.
;; Solution: The reason that the process is now theta(n) is that the expression
;; (expmod base (/ exp 2) m) must now be evaluated twice for every call while
;; using square allows for the expression to be evaluted only once.

;; Exercise 1.27: Demonstrate that the Carmichael numbers listed in Footnote
;; 1.47 really do fool the Fermat test. That is, write a procedure that takes an
;; integer n and tests whether a^n is congruent to a modulo n for every a < n,
;; and try your procedure on the given Carmichael numbers.
;; Solution:
(define (square x) (* x x))
(define (expmod base exp m)
  (cond [(= exp 0) 1]
        [(even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m)]
        [else
         (remainder (* base (expmod base (- exp 1) m))
                    m)]))
(define (test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (loop a)
    (cond [(= a 0) true]
          [(try-it a) (loop (- a 1))]
          [else false]))
  (loop n))

;; Exercise 1.28:
;; One variant of the Fermat test that cannot be fooled is called the
;; Miller-Rabin test (Miller 1976; Rabin 1980). This starts from an alternate
;; form of Fermat’s Little Theorem, which states that if n is a prime number and
;; a is any positive integer less than n, then a raised to the (n-1)-st power is
;; congruent to 1 modulo n. To test the primality of a number n by the
;; Miller-Rabin test, we pick a random number a < n and raise a to the (n-1)-st
;; power modulo n using the expmod procedure. However, whenever we perform the
;; squaring step in expmod, we check to see if we have discovered a "nontrivial
;; square root of 1 modulo n," that is, a number not equal to 1 or n-1 whose
;; square is equal to 1 modulo n. It is possible to prove that if such a
;; nontrivial square root of 1 exists, then n is not prime. It is also possible
;; to prove that if n is an odd number that is not prime, then, for at least
;; half the numbers a < n, computing a^(n-1) in this way will reveal a nontrivial
;; square root of 1 modulo n. (This is why the Miller-Rabin test cannot be
;; fooled.)  Modify the expmod procedure to signal if it discovers a nontrivial
;; square root of 1, and use this to implement the Miller-Rabin test with a
;; procedure analogous to fermat-test. Check your procedure by testing various
;; known primes and non-primes. Hint: One convenient way to make expmod signal
;; is to have it return 0.
;; Solution:
(define (square x) (* x x))
(define (expmod base exp m)
  (cond [(= exp 0) 1]
        [(even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m)]
        [else
         (remainder (* base (expmod base (- exp 1) m))
                    m)]))

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

;; Library
(define (square x) (* x x))
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
(define (average x y)
  (/ (+ x y) 2))

;; Exercise 1.35: Show that the golden ratio phi (Section 1.2.2) is a
;; fixed point of the transformation x -> 1 + 1/x, and use this fact to
;; compute phi by means of the fixed-point procedure.
;; Solution: Let f(x) = 1 + 1/x and remember that φ = (1+sqrt(5))/2.
;; Then plug phi into f and do some algebra.
(fixed-point (lambda (y) (+ 1 (/ 1 y)))
             1.0)

;; Exercise 1.36: Modify `fixed-point` so that it prints the sequence of
;; approximations it generates, using the `newline` and `display` primitives
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
(fixed-point-print (lambda (x) (average x (/ (log 1000) (log x))))
                   2.0)
;; Without average damping
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

;; with average damping
;; 2.0
;; 5.9828921423310435
;; 4.922168721308343
;; 4.628224318195455
;; 4.568346513136242
;; 4.5577305909237005
;; 4.555909809045131
;; 4.555599411610624
;; 4.5555465521473675
;; 4.555537551999825

;; Exercise 1.37:
;; a. An infinite continued fraction is an expression of the form shown
;; in the book. As an example, one can show that the infinite continued
;; fraction expansion with the N_i and the D_i all equal to 1 produces
;; 1/φ, where φ is the golden ratio (described in Section 1.2.2). One
;; way to approximate an infinite continued fraction is to truncate the
;; expansion after a given number of terms. Such a truncation -- a
;; so-called k-term finite continued fraction -- has the form shown in
;; the book. Suppose that n and d are procedures of one argument (the
;; term index i) that return the N_i and D_i of the terms of the
;; continued fraction. Define a procedure `cont-frac` such that
;; evaluating `(cont-frac n d k)` computes the value of the k-term
;; finite continued fraction. Check your procedure by approximating 1/φ
;; using
;; (cont-frac (lambda (i) 1.0)
;;            (lambda (i) 1.0)
;;            k)
;; for successive values of k. How large must you make k in order to get
;; an approximation that is accurate to 4 decimal places?
;; Solution: The function is below. k should be 12 to get an
;; approximation that is accurate to 4 decimal places
(define (cont-frac-recur n d k)
  (define (recur i)
    (let ([n-i (n i)]
          [d-i (d i)])
      (if (= i k)
          (/ n-i d-i)
          (/ n-i (+ d-i (recur (+ i 1)))))))
  (recur 1))

;; b. If your cont-frac procedure generates a recursive process, write one
;; that generates an iterative process. If it generates an iterative
;; process, write one that generates a recursive process.
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1)
              (/ (n i)
                 (+ (d i) result)))))
  (iter k 0))

(define cont-frac cont-frac-iter)
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           12)

;; Exercise 1.38: In 1737, the Swiss mathematician Leonhard Euler published
;; a memoir De Fractionibus Continuis, which included a continued fraction
;; expansion for e − 2, where e is the base of the natural logarithms. In
;; this fraction, the N_i are all 1, and the D_i are successively 1, 2, 1,
;; 1, 4, 1, 1, 6, 1, 1, 8, .... Write a program that uses your cont-frac
;; procedure from Exercise 1.37 to approximate e, based on Euler’s
;; expansion.
(define (euler-number)
  (+ 2
     (cont-frac (lambda (i) 1.0)
                (lambda (i)
                  (if (= (remainder i 3) 2)
                      (* (quotient (+ i 1) 3) 2)
                      1))
                100)))

;; Exercise 1.39: A continued fraction representation of the tangent
;; function was published in 1770 by the German mathematician J.H. Lambert:
;; (see expression in the book) where x is in radians. Define a procedure
;; (tan-cf x k)
;; that computes an approximation to the tangent function based on
;; Lambert’s formula. k specifies the number of terms to compute, as in
;; Exercise 1.37.
(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (- (square x))))
             (lambda (i) (- (* 2 i) 1))
             k))
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
