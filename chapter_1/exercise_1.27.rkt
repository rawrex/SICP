#lang sicp

(define (carmichael-fools-fermat? n)
  (define (fast-prime? n times)
    (cond ((= times 0) true)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else false)))
  (define (fermat-test x)
    (= (expmod x n n) x))
  (define (iter i)
    (cond ((= i n) #f)
          ((not (fermat-test i)) (carmichael-fools-fermat? (inc i)))
          (else #t)))
  (iter 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (square x)
  (* x x))

(carmichael-fools-fermat? 1105)