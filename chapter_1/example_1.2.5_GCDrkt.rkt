#lang sicp

; Iterative implementation of Euclid's Algorithm for finding GCD

(define (GCD a b)
  (if (= b 0)
      a
      (GCD b (remainder a b))))

; Tests
(GCD 28 16)
(GCD 120 45)
(GCD 206 40)