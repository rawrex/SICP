#lang sicp

(define (prime? n)
  (fast-prime? n 3))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ;((fermat-test n) (fast-prime? n (- times 1)))
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (is-nontrivial-sqrt x n)
  (if (and
       (or (not (= 1 x)) (not (= x (- n 1))))
       (= (remainder (square x) n) 1))
      0
      1))
  
(define (square x)
  (* x x))