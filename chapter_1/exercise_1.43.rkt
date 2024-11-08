#lang sicp

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeat f n)
  (if (= n 1)
    (lambda (x) (f x))
    (compose f (repeat f (dec n)))))

((repeat (lambda (x) (* x x)) 2) 5)
