#lang sicp

(define (increment x)
  (+ x 1))

(define (decrement x)
  (- x 1))

(define (sum-recursive a b)
  (if (= a 0)
      b
      (increment (sum-recursive (decrement a) b))))

(define (sum-iter a b)
  (if (= a 0)
      b
      (sum-iter (decrement a) (increment b))))

(sum-recursive 4 5)
(sum-iter 4 5)