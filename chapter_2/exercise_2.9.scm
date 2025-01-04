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
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

(define (width interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (make-interval a b)
  (cons a b))

(define interval-a (make-interval 1.0 7.0))
(define interval-b (make-interval -1.0 3.0))

(define width-a (width interval-a))
(define width-b (width interval-b))

;; Addition and subtraction tests
(define width-a-plus-b (width (add-interval interval-a interval-b)))
(define width-a-minus-b (width (sub-interval interval-b interval-a)))

(display "sum of widths: ")
(display (+ width-b width-a))
(display " | width of sum: ")
(display width-a-plus-b)
(newline)
(display "diff of widths: ")
(display (- width-b width-a))
(display " | width of diff: ")
(display width-a-minus-b)
(newline)
(newline)

;; Multiplication and division tests
(define width-a-mul-b (width (mul-interval interval-a interval-b)))
(define width-a-div-b (width (div-interval interval-b interval-a)))

(display "mul of widths: ")
(display (* width-b width-a))
(display " | width of mul: ")
(display width-a-mul-b)
(newline)
(display "div of widths: ")
(display (/ width-b width-a))
(display " | width of div: ")
(display width-a-div-b)