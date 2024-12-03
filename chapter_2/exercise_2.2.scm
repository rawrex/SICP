#lang sicp

;; Point
(define (make-point x y)
  (cons x y))

(define (point-x point)
  (car point))

(define (point-y point)
  (cdr point))


;; Segment
(define (make-segment start end)
  (cons start end))

(define (segment-start segment)
  (car segment))

(define (segment-end segment)
  (cdr segment))


;; Segment operations
(define (segment-midpoint segment)
  (let* ((start (segment-start segment))
        (end (segment-end segment))
        (start-x (point-x start))
        (start-y (point-y start))
        (end-x (point-x end))
        (end-y (point-y end))
        (mid-x (/ (+ start-x end-x) 2.0))
        (mid-y (/ (+ start-y end-y) 2.0)))
    (make-point mid-x mid-y)))


;; Utils
(define (print-point p)
  (newline)
  (display "(")
  (display (point-x p))
  (display ", ")
  (display (point-y p))
  (display ")"))


;; Main
(define p1 (make-point 1.0 1.0))
(define p2 (make-point 3.0 3.0))
(define segment1 (make-segment p1 p2))
(define mid-p1 (segment-midpoint segment1))
(print-point mid-p1)