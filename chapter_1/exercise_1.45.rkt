#lang sicp

(define (average-damp f)
  (define (average x y)
    (/ (+ x y) 2))
  (lambda (x) (average x (f x))))

(define (repeat f n)
  (define (compose f g)
    (lambda (x) (f (g x))))
  (if (= n 1)
    (lambda (x) (f x))
    (compose f (repeat f (dec n)))))

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define (root-4 x)
  (fixed-point ((repeat average-damp 2) (lambda (y) (/ x (* y y y)))) 1.0))

(root-4 16)
