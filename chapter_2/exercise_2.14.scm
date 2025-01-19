#lang sicp

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


;; Exercise 2.14
(define (parallel-1 resistor-a resistor-b)
  (div-interval (mul-interval resistor-a resistor-b) (add-interval resistor-a resistor-b)))

(define (parallel-2 resistor-a resistor-b)
  (let ((unit (make-interval 1.0 1.0)))
    (div-interval unit (add-interval (div-interval unit resistor-a) (div-interval unit resistor-b)))))

;; Tests
(define A (make-center-percent 0.5 5))
(define B (make-center-percent 2.0 5))


;; parallel-1 directly computes R_a*R_b,
;; which amplifies the uncertainty through multiplication.
;;
;; parallel-2 computes reciprocals and their sum first,
;; which indirectly reduces the range of the final interval
;; due to the specific way uncertainties combine in reciprocal operations
(parallel-1 A B)
(parallel-2 A B)