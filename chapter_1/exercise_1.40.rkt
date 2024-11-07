#lang sicp

;;
;; Section 1.1.7
;;

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

;;
;; Section 1.3.3
;;

;; Binary search
(define (search f neg-point pos-point)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.001))
  (define (average a b)
    (/ (+ a b) 2.0))
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
      midpoint
      (let ((test-value (f midpoint)))
        (cond ((positive? test-value) (search f neg-point midpoint))
              ((negative? test-value) (search f midpoint pos-point))
              (else midpoint))))))

;; Half-interval method itself
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value)) (search f a b))
          ((and (negative? b-value) (positive? a-value)) (search f b a))
          (else (error "Values are not of opposite sign" a b)))))

;; Fixed-points finder
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


;;
;; Section 1.3.4
;;

(define (average-damp f)
  (lambda (x) (average x (f x))))

;; Average damping square root
(define (sqrt-average-damp x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (deriv g)
  (let ((dx 0.00001))
    (lambda (x) (/ (- (g (+ x dx)) (g x)) dx))))


