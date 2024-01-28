#lang sicp

(define (square x)
  (* x x))

;; Smallest Divisor Test

(define (prime-a? n)
  (define (divides? divisor number)
    (= (remainder number divisor) 0))
  (define (find-divisor n test-divisor)
    (cond
      ((> (square test-divisor) n) n)
      ((divides? test-divisor n) test-divisor)
      (else (find-divisor n (inc test-divisor)))))
  (define (smallest-divisor n)
    (find-divisor n 2))
  (= (smallest-divisor n) n))


;; The Fermat Test

(define (prime-b? n)
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
  (fast-prime? n 3))


