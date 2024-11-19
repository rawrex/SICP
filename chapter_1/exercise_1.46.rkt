#lang sicp


;; Misc 
(define (square x)
  (* x x))
(define (average x y)
  (/ (+ x y) 2))

;; Newton's square root, section 1.1.7
(define (sqrt-newton-old x)
  (define (sqrt-iter guess x)
    (define tolerance 0.001)
    (define (improve guess x)
      (average guess (/ x guess)))
    (define (good-enough? guess x)
      (< (abs (- (square guess) x)) tolerance))
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

;; Fixed-points finder, section 1.3.3
(define (fixed-point-old f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

;; The exercise 1.46 
(define (iterative-improve enough? improve)
  (lambda (guess)
    (if (enough? guess)
        guess
        ((iterative-improve enough? improve) (improve guess)))))

(define (sqrt-newton-new x)
  ;; New Newton's method, based on iterative-improve
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  ((iterative-improve good-enough? improve) 1.0))

(define (sqrt-fp-new x)
  ;; New fixed-point finder, based on iterative-improve
  (define (fixed-point f init-guess)
    (define (improve guess) (f guess))
    (define (good-enough? guess)
      (< (abs (- guess (f guess))) 0.00001))
    ((iterative-improve good-enough? improve) init-guess))
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(define (sqrt-fp-old x)
  (fixed-point-old (lambda (y) (average y (/ x y))) 1.0))

;; Tests
(sqrt-newton-old 2)
(sqrt-newton-new 2)
(sqrt-fp-old 2)
(sqrt-fp-new 2)