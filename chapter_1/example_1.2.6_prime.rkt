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

(define (prime? n)
  (= (smallest-divisor n) n))