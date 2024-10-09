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
    (= (expmod-miller-rabin a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod-miller-rabin base exp m)
  (cond ((= exp 0) 1)
    ((even? exp) (remainder-nontrivial-sqrt (square (expmod-miller-rabin base (/ exp 2) m)) m))
    (else (remainder (* base (expmod-miller-rabin base (- exp 1) m)) m))))

(define (remainder-nontrivial-sqrt x n)
  (if (and
       (or (not (= 1 x)) (not (= x (- n 1))))
       (= (remainder (square x) n) 1))
    0
    (remainder (square x) n)))

(define (square x)
  (* x x))

;; Test
(define smallest-carmichael-number 561)
(prime? smallest-carmichael-number)
