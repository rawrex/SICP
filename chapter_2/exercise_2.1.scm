#lang sicp

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define numer car)
(define denom cdr)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat-old n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (make-rat n d)
  (let ((abs-n (abs n))
        (abs-d (abs d))
        (g (gcd (abs n) (abs d))))
    (cond ((and (>= n 0) (>= d 0)) (cons (/ n g) (/ d g)))
          ((and (< n 0) (< d 0)) (cons (/ abs-n g) (/ abs-d g)))
          (else (cons (- (/ abs-n g)) (/ abs-d g))))))

;; Tests
(define half-a (make-rat 1 2))
(define half-b (make-rat -1 2))
(define half-c (make-rat 1 -2))
(define half-d (make-rat -1 -2))

(print-rat half-a)
(print-rat half-b)
(print-rat half-c)
(print-rat half-d)
