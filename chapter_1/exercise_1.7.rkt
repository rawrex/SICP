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

;; Examples of hitting the precision issue
;; 
;; The one below won't terminate
;; (old-sqrt-iter 1.0 12345678901231)
;; The one below will
(old-sqrt-iter 1.0 12345678901232)
;; Must be 0.03, but I get 0.0403... on my machine
(old-sqrt-iter 1.0 0.0009)


;; Let's exploit the machine's precision then
(define (new-good-enough? guess x)
  (= guess (improve guess x)))

(define (sqrt-iter guess x)
  (if (new-good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(sqrt-iter 1.0 81)