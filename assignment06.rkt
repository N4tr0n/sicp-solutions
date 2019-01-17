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
      (display " Not prime\n")))

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
