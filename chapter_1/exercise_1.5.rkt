#lang sicp

(define (infinite-recursion) (infinite-recursion))

(define (test zero x)
  (if (= zero 0)
      0
      x))

(test 0 (infinite-recursion))