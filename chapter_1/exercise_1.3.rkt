#lang sicp

(define (square x)
  (* x x))

(define (sum-squares x y)
  (+ (square x) (square y)))

;; Does not consider all the possible cases
(define (function a b c)
  (cond ((and (> a b) (> b c)) (sum-squares a b))
         ((and (> a b) (> c b)) (sum-squares a c))
         ((and (> b a) (> c a)) (sum-squares b c))
         (else -1)))

(function 1 2 3)
(function 3 2 1)
(function 3 1 2)

(function 3 3 2)
(function 3 2 3)
(function 2 3 3)

(function 3 3 3)