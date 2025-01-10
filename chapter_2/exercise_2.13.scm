#lang sicp

;; For small percentage tolerances,
;; the approximate percentage tolerance of the product of two intervals
;; is the sum of the percentage tolerances of the factors.
;;
;; This approximation holds under the assumption that all numbers are positive and tolerances are small.


(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (= (width y) 0)
      (error "Division by zero width interval: " y)
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

;; Interval based constructor and selectors
(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (make-interval a b)
  (cons a b))

;; Center-width based constructor and selectors
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2))

;; Percentage based constructor and selectors
(define (make-center-percent center p)
  (let ((offset (* p (/ center 100.0))))
    (make-interval (- center offset) (+ center offset))))

(define (percent interval)
  (* 100.0 (- (center interval) (lower-bound interval))))
