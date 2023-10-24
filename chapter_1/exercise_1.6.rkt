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

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))


;; Our custom non-special-form "if" operator
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;; Let's test our new if in the wild
;; The trouble will come from the fact that while in substitution model of evaluation
;; we pass the "new-sqrt-iter" to the "new-if" function (which is a funciton in this case)
;; which will in turn call the "new-if" function, which again will call the "new-sqrt-iter",
;; while trying to perform its conditional role
;;
(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (new-sqrt-iter (improve guess x) x)))