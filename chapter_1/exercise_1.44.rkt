#lang sicp

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeat f n)
  (if (= n 1)
    (lambda (x) (f x))
    ; (compose f (repeat f (dec n)))))
    (lambda (x) (f ((repeat f (dec n)) x)))))

(define (smoothed f)
  (define (average a b c)
    (/ (+ a b c) 3))
  (define dx 0.0001)
  (lambda (x) (average (f (- x dx)) (f x) (f (+ x dx)))))

(define (n-smoothed f n)
  ;(repeat (smoothed f) n)) ;; -> smoothed(f(smoothed(f)) [Incorrect] 
  ((repeat smoothed n) f))  ;; -> smoothed(smoothed(f))   [Correct]

((n-smoothed sqrt 1) 2)
((n-smoothed sqrt 10) 2)
