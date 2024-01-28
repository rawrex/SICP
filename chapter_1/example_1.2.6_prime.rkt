#lang sicp

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond
    ;; If we take test-divisor test-divisor times,
    ;; then, if we are going in ascending order,
    ;; test-divisor should already been shown to be a divisor of n.
    ;; 
    ;; In other words, if d is a divisor of n, then so is the n/d
    ;; But d and the n/d cannot both be greater than the sqrt(n)
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (inc test-divisor)))))

(define (square x)
  (* x x))

(define (divides? divisor number)
  (= (remainder number divisor) 0))

(define (slow-prime? n)
  (= (smallest-divisor n) n))


;; The Fermat Test

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  (fast-prime? n 3))

