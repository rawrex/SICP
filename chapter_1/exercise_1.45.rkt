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

(define (nth-root-test x n damp-counter)
  (define (f y)
    (/ x (expt y (- n 1))))
  (fixed-point ((repeat average-damp damp-counter) f) 1.0))

;; Experiments
(define test-value-1 12) 
(define test-value-2 123) 
(define test-value-3 1234)

;; First fail with damp-counter = 2
;(define damp-counter 2)
;(define test-exponent 8)

;; First fail with damp-counter = 3
;(define damp-counter 3)
;(define test-exponent 16)

;; First fail with damp-counter = 4
;(define damp-counter 4)
;(define test-exponent 32)

;; Note: the test-value-3 (1234) was too slow for the 63,
;; But overall it did converge
;(define damp-counter 4)
;(define test-exponent 63)

;(nth-root-test (expt test-value-1 test-exponent) test-exponent damp-counter)
;(nth-root-test (expt test-value-2 test-exponent) test-exponent damp-counter)
;(nth-root-test (expt test-value-3 test-exponent) test-exponent damp-counter)


;; Resulting procedure definition
;;
;; The assumption is that an appropriate damp counter can be deduced as: log_2(n) - 1
;; Or to be correct, as floor of the log of n by the base 2
(define (nth-root x n)
  (define (f y)
    (/ x (expt y (- n 1))))
  (let ((deduced-damp-counter (floor (log n 2))))
    (fixed-point ((repeat average-damp deduced-damp-counter) f) 1.0)))

;; Final tests
(define test-exponent 63)
(nth-root (expt test-value-1 test-exponent) test-exponent)
(nth-root (expt test-value-2 test-exponent) test-exponent)
(nth-root (expt test-value-3 test-exponent) test-exponent)