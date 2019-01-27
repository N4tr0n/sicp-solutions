#lang racket

(provide (all-defined-out))

(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (current-milliseconds) start-time))
      (display " X\n")))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (search-for-primes start end)
  (cond ((= start end) (display "Done\n"))
        ((even? start) (search-for-primes (+ start 1) end))
        (else (begin
                 (timed-prime-test start)
                 (search-for-primes (+ start 1) end)))))

;; 1009, 1013, 1019
;; 10007, 10009, 10037, 10039
;; 100003, 100019, 100043
;; 1000003, 1000033, 1000037, 1000039

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

(define noop #f)
