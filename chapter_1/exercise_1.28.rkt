#lang sicp

(define (prime? n)
  (define (fast-prime? n times)
    (cond ((= times 0) #t)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else #f)))
  (define (fermat-test n)
    (try-it (+ 1 (random (- n 1)))))
  (define (try-it a)
    (= (new-expmod a (- n 1) n) a))
  (fast-prime? n 3))

(define (new-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (new-expmod base (/ exp 2) m)) m))
        (else (remainder (* base (new-expmod base (- exp 1) m)) m))))
        
  