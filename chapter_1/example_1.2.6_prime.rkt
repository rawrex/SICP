#lang sicp

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond
    ;; if we take test-divisor test-divisor times,
    ;; then, if we are going in ascending order,
    ;; test-divisor should already been shown to be a divisor of n
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (inc test-divisor)))))

(define (square x)
  (* x x))

(define (divides? divisor number)
  (= (remainder number divisor) 0))
        