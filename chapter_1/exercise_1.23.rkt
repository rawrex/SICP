#lang sicp

(define (prime-a? n)
  (= (smallest-divisor n) n))

(define (smallest-divisor n)
  (find-divisor n 2))

;; Let's redefine the find-divisor 
(define (find-divisor n test-divisor)
  (define (next x)
    (if (= x 2)
        (inc x)
        (+ x 2)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? divisor number)
  (= (remainder number divisor) 0))

(define (square n)
  (* n n))