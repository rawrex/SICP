#lang sicp

(define tolerance 0.001)

(define (square x)
  (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) tolerance))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (old-sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (old-sqrt-iter (improve guess x) x)))

(define (new-good-enough? guess x)
  (= guess (improve guess x)))

(define (sqrt-iter guess x)
  (if (new-good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(sqrt-iter 1.0 81)