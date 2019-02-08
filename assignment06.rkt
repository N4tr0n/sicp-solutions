#lang sicp

(define (square x) (* x x))

(define (smallest-divisor-1 n) (find-divisor-1 n 2))

(define (smallest-divisor-2 n) (find-divisor-2 n 2))

(define (find-divisor-1 n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor-1 n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (find-divisor-2 n test-divisor)
  (define (next x)
    (cond [(= 2 x) 3]
          [else (+ x 2)]))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor-2 n (next test-divisor)))))

(define (timed-prime-test-1 n)
  (newline)
  (display n)
  (start-prime-test-1 n (runtime)))

(define (start-prime-test-1 n start-time)
  (if (prime?-1 n)
      (report-prime (- (runtime) start-time))))

(define (timed-prime-test-2 n)
  (newline)
  (display "|")
  (display n)
  (start-prime-test-2 n (runtime)))

(define (start-prime-test-2 n start-time)
  (if (prime?-2 n)
      (report-prime (- (runtime) start-time))))


(define (report-prime elapsed-time)
  (display "|")
  (display elapsed-time)
  (display "|")
  (newline))

(define (prime?-1 n)
  (= n (smallest-divisor-1 n)))

(define (prime?-2 n)
  (= n (smallest-divisor-2 n)))

(define (search-for-primes start end)
  (cond ((> start end) (display "\nDone\n"))
        ((even? start) (search-for-primes (+ start 1) end))
        (else (begin
                (timed-prime-test-1 start)
                (search-for-primes (+ start 1) end)))))

(define (expmod base exp m)
  (cond [(= exp 0) 1]
        [(even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m)]
        [else
         (remainder (* base (expmod base (- exp 1) m))
                    m)]))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond [(= times 0) true]
        [(fermat-test n) (fast-prime? n (- times 1))]
        [else false]))

(define (timed-prime-test-3 n)
  (newline)
  (display n)
  (start-prime-test-3 n (runtime)))

(define (start-prime-test-3 n start-time)
  (if (fast-prime? n 4)
      (report-prime (- (runtime) start-time))))


;(timed-prime-test-3 1009)
;(timed-prime-test-3 1013)
;(timed-prime-test-3 1019)
;(timed-prime-test-3 10007)
;(timed-prime-test-3 10009)
;; (timed-prime-test-3 10037)
;; (timed-prime-test-3 100003)
;; (timed-prime-test-3 100019)
;; (timed-prime-test-3 100043)
;; (timed-prime-test-3 1000003)
;; (timed-prime-test-3 1000033)
;; (timed-prime-test-3 1000037)
(display "hello")
(timed-prime-test-3 3)
(timed-prime-test-3 10000019)
(timed-prime-test-3 10000079)
(timed-prime-test-3 10000103)

(define (test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (loop a)
    (cond [(= a 0) true]
          [(try-it a) (loop (- a 1))]
          [else false]))
  (loop n))
