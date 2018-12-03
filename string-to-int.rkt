(define (nah/string->int s)
  (define digit-map
    (hash #\0 0
          #\1 1
          #\2 2
          #\3 3
          #\4 4
          #\5 5
          #\6 6
          #\7 7
          #\8 8
          #\9 9))
  (define (loop remaining power-of-ten acc)
    (cond
     [(> (string-length remaining) 0)
      (let* ([c (string-ref remaining 0)]
             [digit (hash-ref digit-map c)])
        (loop (substring remaining 1)
              (/ power-of-ten 10)
              (+ acc (* digit power-of-ten))))]
     [else acc]))
  (loop s (expt 10 (- (string-length s) 1)) 0))
