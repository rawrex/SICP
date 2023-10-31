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

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (cubert x)
  (cubert-iter 1.0 x))

(sqrt 81)
(cubert 27)

;; Now, let's factor out the common patterns into a separate mechanism

(define (good-enough? guess x improver-function)
  (= guess (improver-function guess x)))

(define (iter guess x improver-function)
  (if (good-enough? guess x improver-function)
      guess
      (iter (improver-function guess x) x improver-function)))

(define (new-sqrt x)
  (iter 1.0 x improve-sqrt))

(define (new-cubert x)
  (iter 1.0 x improve-cubert))

(new-sqrt 81)
(new-cubert 27)