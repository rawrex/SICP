#lang sicp

(define (square x)
  (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

;; Must return 49, as it is a square of 6+1
((compose square inc) 6)
