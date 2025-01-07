#lang sicp

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((x-low (lower-bound x))
        (x-high (upper-bound x))
        (y-low (lower-bound y))
        (y-high (upper-bound y)))
    (cond 
      ;; Both intervals are positive
      ((and (>= x-low 0) (>= y-low 0))
       (make-interval (* x-low y-low) (* x-high y-high)))
      
      ;; x positive, y negative
      ((and (>= x-low 0) (<= y-high 0))
       (make-interval (* x-high y-low) (* x-low y-high)))
      
      ;; x positive, y crosses zero
      ((and (>= x-low 0) (<= y-low 0) (>= y-high 0))
       (make-interval (* x-high y-low) (* x-high y-high)))

      ;; Both intervals are negative
      ((and (<= x-high 0) (<= y-high 0))
       (make-interval (* x-high y-high) (* x-low y-low)))

      ;; x negative, y positive
      ((and (<= x-high 0) (>= y-low 0))
       (make-interval (* x-low y-high) (* x-high y-low)))

      ;; x negative, y crosses zero
      ((and (<= x-high 0) (<= y-low 0) (>= y-high 0))
       (make-interval (* x-low y-high) (* x-low y-low)))

      ;; x crosses zero, y positive
      ((and (<= x-low 0) (>= x-high 0) (>= y-low 0))
       (make-interval (* x-low y-high) (* x-high y-high)))

      ;; x crosses zero, y negative
      ((and (<= x-low 0) (>= x-high 0) (<= y-high 0))
       (make-interval (* x-high y-low) (* x-low y-low)))

      ;; Both intervals cross zero
      (else
       (make-interval (min (* x-low y-high) (* x-high y-low))
                      (max (* x-low y-low) (* x-high y-high)))))))


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