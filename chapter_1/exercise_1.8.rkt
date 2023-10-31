#lang sicp

(define tolerance 0.001)

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve-sqrt guess x)
  (average guess (/ x guess)))

(define (good-enough-sqrt? guess x)
  (= guess (improve-sqrt guess x)))

(define (sqrt-iter guess x)
  (if (good-enough-sqrt? guess x)
      guess
      (sqrt-iter (improve-sqrt guess x) x)))

(define (improve-cubert guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (good-enough-cubert? guess x)
  (= guess (improve-cubert guess x)))

(define (cubert-iter guess x)
  (if (good-enough-cubert? guess x)
      guess
      (cubert-iter (improve-cubert guess x) x)))

(sqrt-iter 1.0 81)
(cubert-iter 1.0 27)