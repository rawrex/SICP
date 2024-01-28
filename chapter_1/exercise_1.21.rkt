#lang sicp

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (inc test-divisor)))))

(define (square x)
  (* x x))

(define (divides? divisor number)
  (= (remainder number divisor) 0))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)
(smallest-divisor 199999)
(smallest-divisor 1999999)
(smallest-divisor 19999999)
(smallest-divisor 199999999)
(smallest-divisor 1999999999)
(smallest-divisor 19999999999)