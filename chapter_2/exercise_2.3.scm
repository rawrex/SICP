#lang sicp

;; Common
(define (square x)
  (* x x))

;; Point
(define (make-point x y)
  (cons x y))

(define (point-x point)
  (car point))

(define (point-y point)
  (cdr point))


;; Segment
(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (segment-start segment)
  (car segment))

(define (segment-end segment)
  (cdr segment))

(define (segment-length segment)
  (sqrt (+
         (square (- (point-x (segment-start segment)) (point-x (segment-end segment))))
         (square (- (point-y (segment-start segment)) (point-y (segment-end segment)))))))


;; Rectangle
;;      a
;;   A-----B
;; d |     | b
;;   D-----C
;;      c

;; Constructor(s)
(define (make-rect point-A point-C)
  (cons point-A point-C))

;; Selectors for vertices
(define (rect-A rect)
  (car rect))
(define (rect-C rect)
  (cdr rect))
(define (rect-B rect)
  (make-point (point-x (rect-C rect)) (point-y (rect-A rect))))
(define (rect-D rect)
  (make-point (point-x (rect-A rect)) (point-y (rect-C rect))))

;; Selectors for sides
(define (rect-a rect)
  (make-segment (rect-A rect) (rect-B rect)))
(define (rect-b rect)
  (make-segment (rect-B rect) (rect-C rect)))
(define (rect-c rect)
  (make-segment (rect-C rect) (rect-D rect)))
(define (rect-d rect)
  (make-segment (rect-D rect) (rect-A rect)))


;; Operations on the rectangles
(define (rect-perimeter rect)
  (+
   (* 2 (segment-length (rect-a rect)))
   (* 2 (segment-length (rect-b rect)))))

(define (rect-area rect)
  (*
   (segment-length (rect-a rect))
   (segment-length (rect-b rect))))
  

;; Utils
(define (print-point p)
  (newline)
  (display "(")
  (display (point-x p))
  (display ", ")
  (display (point-y p))
  (display ")"))


;; Main
(define s (make-segment (make-point 1 1) (make-point 4 1)))
(segment-length s)

(define r (make-rect (make-point 1 1) (make-point 4 3)))
(rect-perimeter r)
(rect-area r)